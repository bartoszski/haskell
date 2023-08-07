#!/bin/bash

# Update the package index and install necessary packages to use apt over HTTPS
sudo apt-get update
sudo apt-get install -y apt-transport-https ca-certificates curl software-properties-common

# Add Docker's official GPG key
curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo gpg --dearmor -o /usr/share/keyrings/docker-archive-keyring.gpg

# Set up the stable Docker repository
echo "deb [arch=amd64 signed-by=/usr/share/keyrings/docker-archive-keyring.gpg] https://download.docker.com/linux/ubuntu $(lsb_release -cs) stable" | sudo tee /etc/apt/sources.list.d/docker.list

# Install Docker engine
sudo apt-get update
sudo apt-get install -y docker-ce docker-ce-cli containerd.io

# Add your user to the docker group to run Docker commands without sudo
sudo usermod -aG docker $USER

# Enable and start the Docker service
sudo systemctl enable docker
sudo systemctl start docker

echo "Docker has been installed successfully. Please log out and log back in to apply group changes and use Docker without sudo."

