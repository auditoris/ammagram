#(@) Ammagram


### 1. Install Docker Desktop
https://www.docker.com/products/docker-desktop

### 2. Download Docker Image for Data Science
docker pull auditoris/ds_dock

### 3. Run the docker image
docker run -Pit --name ds_dock -p 8888:8888 -p 8787:8787 -p 6006:6006 -p 8022:22 --cap-add=SYS_PTRACE --security-opt seccomp=unconfined -v /d/my_docks/:/home/dockeruser/hosthome auditoris/ds_dock

### 4. Download the source code
D:/my_docks (in your folder)

### 5. Access in Browser
#### - Rstudio 
http://127.0.0.1:8787
#### - Python Jupyter
http://127.0.0.1:8888

### 6. Stop the docker image
docker stop ds_dock

### 7. Restart the docker image
docker start ds_dock

