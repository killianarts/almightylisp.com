almightylisp: *.asd *.lisp src/* *.ros static/*
	vend get
	ros build almightylisp.ros
dev:
	sbcl --load load-system.lisp
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
	$(MAKE) purge
backup:
	cp ~/.local/bin/almightylisp ~/.local/bin/almightylisp.backup
recover:
	rm ~/.local/bin/almightylisp
	cp ~/.local/bin/almightylisp.backup ~/.local/bin/almightylisp
	sudo cp almightylisp.service /etc/systemd/system/almightylisp.service
	sudo cp almightylisp.env /etc/systemd/system/almightylisp.env
	sudo systemctl daemon-reload
	sudo systemctl restart almightylisp.service
	$(MAKE) purge
run:
	sudo systemctl start almightylisp.service

# Cloudflare keeps pages for 30 days (see *page-cache*), so empty its cache
# once the new build answers. The file holds CF_ZONE_ID and CF_API_TOKEN.
CLOUDFLARE_ENV ?= $(HOME)/.config/almightylisp/cloudflare.env
APP_URL ?= http://localhost:5000/
purge:
	@test -r $(CLOUDFLARE_ENV) || { echo "No $(CLOUDFLARE_ENV): Cloudflare's cache was not purged."; exit 1; }
	@for i in $$(seq 1 60); do curl -fs -o /dev/null $(APP_URL) && break; sleep 1; done; \
	curl -fs -o /dev/null $(APP_URL) || { echo "The site isn't answering at $(APP_URL): not purging."; exit 1; }
	@. $(CLOUDFLARE_ENV); \
	curl -fsS -X POST "https://api.cloudflare.com/client/v4/zones/$$CF_ZONE_ID/purge_cache" \
	  -H "Authorization: Bearer $$CF_API_TOKEN" -H "Content-Type: application/json" \
	  --data '{"purge_everything":true}' | grep -q '"success":true' \
	  && echo "Cloudflare's cache purged." \
	  || { echo "Cloudflare refused the purge: check CF_ZONE_ID and CF_API_TOKEN."; exit 1; }

