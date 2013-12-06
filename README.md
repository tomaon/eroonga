# eroonga

eroonga: erlang client for [groonga](http://groonga.org/).

## Build

``` sh
$ make build
```

## Example

```erlang
% files/shell.config
[
 {eroonga, [
            {poolboy, [
                       {eroonga_pool,
                        [
                         {size, 1},
                         {max_overflow, 3}
                        ],
                        [
                         {address, "localhost"},
                         {port, 10041},
                         {options, [
                                    {active, false},
                                    {buffer, 293976},
                                    {keepalive, true},
                                    {mode, binary},
                                    {packet, raw},
                                    {recbuf, 146988},
                                    {sndbuf, 146988}
                                   ]},
                         {timeout, 10}
                        ]}
                      ]}
           ]}
].
```

```erl-sh
$ make shell
Erlang R16B02 (erts-5.10.3) [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false] [dtrace]

Eshell V5.10.3  (abort with ^G)
1> eroonga:start().
ok
2> {ok, Pid} = eroonga:connect(eroonga_pool).
{ok,<0.40.0>}
3> eroonga:command(Pid, <<"status">>).
{ok,{[{<<"alloc_count">>,187},
      {<<"starttime">>,1386295698},
      {<<"uptime">>,16563},
      {<<"version">>,<<"3.1.0">>},
      {<<"n_queries">>,0},
      {<<"cache_hit_rate">>,0.0},
      {<<"command_version">>,1},
      {<<"default_command_version">>,1},
      {<<"max_command_version">>,2}]}}
4> eroonga:close(eroonga_pool, Pid).
ok
5> eroonga:stop().
ok
6> 
=INFO REPORT==== 6-Dec-2013::15:44:59 ===
    application: eroonga
    exited: stopped
    type: temporary
```
