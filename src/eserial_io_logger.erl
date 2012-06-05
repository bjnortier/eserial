-module(eserial_io_logger).
-export([info/2, warning/2, error/2]).

info(Template, Args) ->
    log("[INFO] " ++ Template, Args).

warning(Template, Args) ->
    log("[WARN] " ++ Template, Args).

error(Template, Args) ->
    log("[ERR] " ++ Template, Args).

log(Template, Args) ->
    io:format(Template, Args).
