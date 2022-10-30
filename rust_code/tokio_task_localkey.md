# task task localkey

## task_local!

``` rust

/// The task local trace context.
#[derive(Debug)]
pub(crate) struct TraceContext {
    /// The id of the context.
    id: ContextId,

    /// Whether to report the detached spans, that is, spans that are not able to be polled now.
    report_detached: bool,

    /// Whether to report the "verbose" stack trace.
    verbose: bool,

    /// The arena for allocating span nodes in this context.
    arena: Arena<SpanNode>,

    /// The root span node.
    root: NodeId,

    /// The current span node. This is the node that is currently being polled.
    current: NodeId,
}

tokio::task_local! {
    pub(crate) static TRACE_CONTEXT: RefCell<TraceContext>
}

pub(crate) fn with_context<F, R>(f: F) -> R
where
    F: FnOnce(&mut TraceContext) -> R,
{
    TRACE_CONTEXT.with(|trace_context| {
        let mut trace_context = trace_context.borrow_mut();
        f(&mut trace_context)
    })
}

pub(crate) fn try_with_context<F, R>(f: F) -> Option<R>
where
    F: FnOnce(&mut TraceContext) -> R,
{
    TRACE_CONTEXT
        .try_with(|trace_context| {
            let mut trace_context = trace_context.borrow_mut();
            f(&mut trace_context)
        })
        .ok()
}

/// Get the current context. Returns `None` if we're not traced.
///
/// This is useful if you want to check which component or runtime task is calling this function.
pub fn current_context() -> Option<String> {
    try_with_context(|c| c.to_string())
}

```

## scope

``` rust
impl TraceReporter {
    /// Provide a stack tracing context with the `root_span` for the given future. The reporter will
    /// be started along with this future in the current task and update the captured stack trace
    /// report periodically.
    pub async fn trace<F: Future>(
        self,
        future: F,
        root_span: impl Into<SpanValue>,
        TraceConfig {
            report_detached,
            verbose,
            interval,
        }: TraceConfig,
    ) -> F::Output {
        TRACE_CONTEXT
            .scope(
                TraceContext::new(root_span.into(), report_detached, verbose).into(),
                async move {
                    let reporter = async move {
                        let mut interval = tokio::time::interval(interval);
                        interval.set_missed_tick_behavior(tokio::time::MissedTickBehavior::Delay);
                        loop {
                            interval.tick().await;
                            let new_trace = with_context(|c| c.to_report());
                            match self.tx.send(new_trace) {
                                Ok(_) => {}
                                Err(e) => {
                                    tracing::error!(
                                        "Trace report error: failed to send trace: {}",
                                        e
                                    );
                                    futures::future::pending().await
                                }
                            }
                        }
                    };

                    tokio::select! {
                        biased; // always prefer reporting
                        _ = reporter => unreachable!(),
                        output = future => output,
                    }
                },
            )
            .await
    }
}
```

## with_context, try_with_context

``` rust
impl<F: Future, const VERBOSE: bool> Future for StackTraced<F, VERBOSE> {
    type Output = F::Output;

    // TODO: may optionally enable based on the features
    fn poll(self: Pin<&mut Self>, cx: &mut std::task::Context<'_>) -> Poll<Self::Output> {
        let this = self.project();

        // For assertion.
        let old_current = try_with_context(|c| c.current());

        let this_node = match this.state {
            StackTracedState::Initial(span) => {
                match try_with_context(|c| (c.id(), c.verbose() >= VERBOSE)) {
                    // The tracing for this span is disabled according to the verbose configuration.
                    Some((_, false)) => {
                        *this.state = StackTracedState::Disabled;
                        return this.inner.poll(cx);
                    }
                    // First polled
                    Some((current_context, true)) => {
                        // First polled, push a new span to the context.
                        let node = with_context(|c| c.push(std::mem::take(span)));
                        *this.state = StackTracedState::Polled {
                            this_node: node,
                            this_context: current_context,
                        };
                        node
                    }
                    // Not in a context
                    None => return this.inner.poll(cx),
                }
            }
            StackTracedState::Polled {
                this_node,
                this_context,
            } => {
                match try_with_context(|c| c.id()) {
                    // Context correct
                    Some(current_context) if current_context == *this_context => {
                        // Polled before, just step in.
                        with_context(|c| c.step_in(*this_node));
                        *this_node
                    }
                    // Context changed
                    Some(_) => {
                        tracing::warn!("stack traced future is polled in a different context as it was first polled, won't be traced now");
                        return this.inner.poll(cx);
                    }
                    // Out of context
                    None => {
                        tracing::warn!("stack traced future is not polled in a traced context, while it was when first polled, won't be traced now");
                        return this.inner.poll(cx);
                    }
                }
            }
            StackTracedState::Ready => unreachable!("the traced future should always be fused"),
            StackTracedState::Disabled => return this.inner.poll(cx),
        };

        // The current node must be the this_node.
        assert_eq!(this_node, with_context(|c| c.current()));

        let r = match this.inner.poll(cx) {
            // The future is ready, clean-up this span by popping from the context.
            Poll::Ready(output) => {
                with_context(|c| c.pop());
                *this.state = StackTracedState::Ready;
                Poll::Ready(output)
            }
            // Still pending, just step out.
            Poll::Pending => {
                with_context(|c| c.step_out());
                Poll::Pending
            }
        };

        // The current node must be the same as we started with.
        assert_eq!(old_current.unwrap(), with_context(|c| c.current()));

        r
    }
}
```

## TraceContext method

``` rust
impl TraceContext {
    /// Create a new stack trace context with the given root span.
    pub fn new(root_span: SpanValue, report_detached: bool, verbose: bool) -> Self {
        static ID: AtomicU64 = AtomicU64::new(0);
        let id = ID.fetch_add(1, Ordering::SeqCst);

        let mut arena = Arena::new();
        let root = arena.new_node(SpanNode::new(root_span));

        Self {
            id,
            report_detached,
            verbose,
            arena,
            root,
            current: root,
        }
    }

    /// Get the count of active span nodes in this context.
    #[cfg(test)]
    pub fn active_node_count(&self) -> usize {
        self.arena.iter().filter(|n| !n.is_removed()).count()
    }

    /// Get the report of the current state of the stack trace.
    pub fn to_report(&self) -> StackTraceReport {
        let report = format!("{}", self);
        StackTraceReport {
            report,
            capture_time: std::time::Instant::now(),
        }
    }

    /// Push a new span as a child of current span, used for future firstly polled.
    ///
    /// Returns the new current span.
    pub fn push(&mut self, span: SpanValue) -> NodeId {
        let child = self.arena.new_node(SpanNode::new(span));
        self.current.append(child, &mut self.arena);
        self.current = child;
        child
    }

    /// Step in the current span to the given child, used for future polled again.
    ///
    /// If the child is not actually a child of the current span, it means we are using a new future
    /// to poll it, so we need to detach it from the previous parent, and attach it to the current
    /// span.
    pub fn step_in(&mut self, child: NodeId) {
        if !self.current.children(&self.arena).contains(&child) {
            // Actually we can always call this even if `child` is already a child of `current`.
            self.current.append(child, &mut self.arena);
        }
        self.current = child;
    }

    /// Pop the current span to the parent, used for future ready.
    ///
    /// Note that there might still be some children of this node, like `select_stream.next()`.
    /// The children might be polled again later, and will be attached as the children of a new
    /// span.
    pub fn pop(&mut self) {
        let parent = self.arena[self.current]
            .parent()
            .expect("the root node should not be popped");
        self.remove_and_detach(self.current);
        self.current = parent;
    }

    /// Step out the current span to the parent, used for future pending.
    pub fn step_out(&mut self) {
        let parent = self.arena[self.current]
            .parent()
            .expect("the root node should not be stepped out");
        self.current = parent;
    }

    /// Remove the current span and detach the children, used for future aborting.
    ///
    /// The children might be polled again later, and will be attached as the children of a new
    /// span.
    pub fn remove_and_detach(&mut self, node: NodeId) {
        node.detach(&mut self.arena);
        // Removing detached `node` makes children detached.
        node.remove(&mut self.arena);
    }

    /// Get the context id.
    pub fn id(&self) -> ContextId {
        self.id
    }

    /// Get the current span node id.
    pub fn current(&self) -> NodeId {
        self.current
    }

    /// Whether the verbose span should be traced.
    pub fn verbose(&self) -> bool {
        self.verbose
    }
}
```

copy from https://github.com/risingwavelabs/risingwave
