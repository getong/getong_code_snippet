# persistent_term
erlang adds persistent_term module since 21.2, and its syntax just likes the process dictionary, but the function just likes ets table.

use persistent_term with great care.
see [persistent-term](https://speakerdeck.com/ckampfe/persistent-term)

>> Think like this! Write once, read many
>> Instead of the generate module in runtime hack!
>> Expensive put and low cost get
>> No copying of the data on get

``` erlang
persistent_term:put(myapp_calls, Cref).
Cref = persistent_term:get(myapp_calls).
counters:get(Cref, 1).
```
