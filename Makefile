lib: osp_broker.erl osp_socket.erl osp.erl osp_mnesia.erl
	erlc osp_*.erl
	erlc osp.erl
clean:
	-rm osp_*.beam *~ osp.beam erl_crash.dump
