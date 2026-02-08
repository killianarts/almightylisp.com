almightylisp: *.asd *.lisp src/* *.ros public/*
	ros build almightylisp.ros
dev: 
	tw -i public/css/input.css -o public/css/almightylisp.css --watch
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
run:
	sudo systemctl start almightylisp.service

