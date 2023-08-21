docker run --rm -p 8888:8888 -v "%cd%":/home/jovyan/src --name jh ihaskell

docker run --rm -p 8000:8888 -v "$PWD":/home/jovyan/src gibiansky/ihaskell

docker run --rm -p 8000:8888 -v ihaskell:/home/jovyan/src gibiansky/ihaskell


docker run --rm -p 8000:8888 -v ~/dev:/home/jovyan/src gibiansky/ihaskell

http://52.215.182.50:8888/?token=225ce662e634ede59cfbd2c32217452e6e59a2125f4b791b

docker run -d -p 80:9000 --name=portainer --restart=always -v /var/run/docker.sock:/var/run/docker.sock portainer/portainer-ce




git remote add origin git@github.com:bartoszski/haskell.git


git push --set-upstream origin master

eval "$(ssh-agent -s)"
chmod 600 ~/.ssh/git_id_rsa
ssh-add ~/.ssh/git_id_rsa  # Use the correct path to your SSH key

git push -u origin main
