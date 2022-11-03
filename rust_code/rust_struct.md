# rust strudct


## copy or move of sturct

>>>
In general any struct member access x.a moves a out of the struct -- except if you take a reference to it, explicitly (&x.a) or implicitly (x.a.method() where fn method(&self)), or if a's type is Copy.

copy from [Why is field access alone considered a move here](https://www.reddit.com/r/rust/comments/io9udn/why_is_field_access_alone_considered_a_move_here/)
