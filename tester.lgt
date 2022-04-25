:- initialization((
    logtalk_load(meta(loader)),
    logtalk_load(types(loader)),
    logtalk_load(registers),
    logtalk_load(machine),
    logtalk_load(parser),
    logtalk_load(lgtunit(loader)),
    logtalk_load(test_registers, [hook(lgtunit)]),
    logtalk_load(test_parser, [hook(lgtunit)]),
    logtalk_load(test_machine, [hook(lgtunit)]),
    lgtunit::run_test_sets([
         test_registers,
	 test_parser,
	 test_machine
    ])
)).