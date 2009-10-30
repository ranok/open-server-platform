osp:
	erlc osp_servlet.erl
	erlc -o ../ebin *.erl
	rm osp_servlet.beam
hipe:
	erlc -smp +"[native, {hipe, [o3]}]" osp_servlet.erl
	erlc -smp +"[native, {hipe, [o3]}]" -o ../ebin *.erl
	rm osp_servlet.beam
clean:
	-rm ../ebin/osp*.beam *~  ../erl_crash.dump ../osp_rel* ../mnesia/*
docs:
	cd .. && erl -pa ./ebin -s osp gen_docs
