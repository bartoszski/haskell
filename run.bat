docker run --rm -p 8888:8888 -v "%cd%":/home/jovyan/src --name jh ihaskell

docker run --rm -p 8888:8888 -v "%cd%":/home/jovyan/work --name learn-you-a-haskell ghcr.io/ihaskell/ihaskell-notebook:master jupyter lab --ServerApp.token=''
docker run --rm -p 8888:8888 -v "%cd%":/home/jovyan/pwd  --name ihaskell_notebook ghcr.io/ihaskell/ihaskell-notebook:master jupyter lab --ServerApp.token=''


-- lab on ec2 
docker run --rm -p 8000:8888 -v "$PWD":/home/jovyan/work  --name ihaskell_notebook ghcr.io/ihaskell/ihaskell-notebook:master jupyter lab --ServerApp.token='' --ip=0.0.0.0
docker run --rm -p 8000:8888 -v haskell2:/home/jovyan/work  --name ihaskell_notebook ghcr.io/ihaskell/ihaskell-notebook:master jupyter lab --ServerApp.token='' --ip=0.0.0.0

docker run --rm -p 8000:8888 -v "$PWD":/home/jovyan/src gibiansky/ihaskell
docker run --rm -p 8000:8888 -v ihaskell:/home/jovyan/src gibiansky/ihaskell




docker run -d -p 80:9000 --name=portainer --restart=always -v /var/run/docker.sock:/var/run/docker.sock portainer/portainer-ce



git remote add origin git@github.com:bartoszski/haskell.git
git push --set-upstream origin master

eval "$(ssh-agent -s)"
chmod 600 ~/.ssh/git_id_rsa
ssh-add ~/.ssh/git_id_rsa  # Use the correct path to your SSH key

git push -u origin main