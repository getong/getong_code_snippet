# relx
>{release, {sexpr, "0.0.1"},
> [sexpr,
>  %% There are two syntaxes for constraints.
>  %% The first is the tuple syntax shown here.
>  {erlware_commons, "0.8.0", '<='}]}.
>
>{release, {sexpr, "0.0.2"},
> [sexpr,
>
>  %% This is the second constraint syntax, it is interchangeable with the tuple
>  %% syntax and its up to you which you find more readable/usable.
>  "erlware_commons>=0.8.1",
>
>  %% You can put the release load types in the release spec here in exactly the
>  %% same way that you can do it for a normal relfile. The syntax is
>  %% {<constraint>, <type>}.
>  {neotoma, load}]}.

The [relx wiki](https://github.com/erlware/relx/wiki) is a good place to start.
