:- initialization((
    logtalk_load(meta(loader)),
    logtalk_load(types(loader)),
    logtalk_load(registers),
    logtalk_load(lgtunit(loader)),
    logtalk_load(test_registers, [hook(lgtunit)]),
    lgtunit::run_test_sets([
        test_registers
    ])
)).