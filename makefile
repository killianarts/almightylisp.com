almightylisp: *.asd *.lisp src/* *.ros static/*
	vend get
	ros build almightylisp.ros
install:
	mv almightylisp ~/.local/bin/almightylisp
	sudo cp almightylisp.service /etc/systemd/system/almightylisp.service
	sudo cp almightylisp.env /etc/systemd/system/almightylisp.env
	sudo systemctl daemon-reload
	sudo systemctl enable almightylisp.service
update:
	rm ~/.local/bin/almightylisp
	mv almightylisp ~/.local/bin/almightylisp
	sudo cp almightylisp.service /etc/systemd/system/almightylisp.service
	sudo cp almightylisp.env /etc/systemd/system/almightylisp.env
	sudo systemctl daemon-reload
	sudo systemctl restart almightylisp.service
backup:
	cp ~/.local/bin/almightylisp ~/.local/bin/almightylisp.backup
recover:
	rm ~/.local/bin/almightylisp
	cp ~/.local/bin/almightylisp.backup ~/.local/bin/almightylisp
	sudo cp almightylisp.service /etc/systemd/system/almightylisp.service
	sudo cp almightylisp.env /etc/systemd/system/almightylisp.env
	sudo systemctl daemon-reload
	sudo systemctl restart almightylisp.service
run:
	sudo systemctl start almightylisp.service

