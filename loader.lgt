:- initialization((
    logtalk_load(meta(loader)),
    logtalk_load(types(loader)),
    logtalk_load(registers),
    % logtalk_load(parser),
    logtalk_load(machine),
    logtalk_load(mipsie)
)).