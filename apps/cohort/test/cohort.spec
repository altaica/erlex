{node, magnumopus, 'magnumopus@localhost'}.
{node, obsequilis, 'obsequilis@localhost'}.

{init, [magnumopus, obsequilis], [{node_start, [{monitor_master, true}]}]}.

