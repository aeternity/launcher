%%% Conf meta record type.

-record(conf_meta,
        {id   = 0  :: integer(),
         name = "" :: string(),
         path = "" :: string(),
         memo = "" :: string()}).
