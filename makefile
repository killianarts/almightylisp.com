almightylisp: *.asd *.lisp src/*.lisp src/*/*.lisp
	ros build almightylisp.ros
dev: 
	tw -i public/css/input.css -o public/css/almightylisp.css --watch
install:
	mv almightylisp ~/.local/bin/almightylisp
	sudo cp almightylisp.service /etc/systemd/system/almightylisp.service
	sudo cp almightylisp.env /etc/systemd/system/almightylisp.env
	sudo systemctl daemon-reload
	sudo systemctl enable almightylisp.service
run:
	sudo systemctl start almightylisp.service

