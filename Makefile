REMOTE_PATH=dnplayer-server
USER=host
SERVER=dbpmail.net

all:
	cabal install -fdevelopment && ./.cabal-sandbox/bin/dnplayer-server

deploy:
	cabal install
	scp .cabal-sandbox/bin/dnplayer-server $(USER)@$(SERVER):$(REMOTE_PATH)
	rsync --checksum -avz -e ssh static/* $(USER)@$(SERVER):$(REMOTE_PATH)/static
	rsync --checksum -avz -e ssh snaplets/* $(USER)@$(SERVER):$(REMOTE_PATH)/snaplets

migrate:
	rsync --checksum -ave 'ssh '  migrations/* $(USER)@$(SERVER):$(REMOTE_PATH)/migrations
	ssh $(USER)@$(SERVER) "/var/www/dnplayer-server/moo.sh upgrade"
