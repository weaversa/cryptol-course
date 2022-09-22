docker build - -t ghcr.io/weaversa/cryptol-course:2.13 < Dockerfile.2.13
docker push ghcr.io/weaversa/cryptol-course:2.13

docker build - -t ghcr.io/weaversa/cryptol-course:gitpod < Dockerfile.gitpod
docker push ghcr.io/weaversa/cryptol-course:gitpod
