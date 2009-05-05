osp:
	erlc osp_servlet.erl
	erlc -o ../ebin *.erl
	rm osp_servlet.beam
clean:
	-rm ../ebin/osp*.beam *~  ../erl_crash.dump ../osp_rel*
docs:
	cd .. && erl -pa ./ebin -detached -s osp gen_docs
