DOCKER_TAG = l1drm/fedi-emoji-tool:latest

build-local:
	cabal build

build-docker:
	docker buildx build --tag ${DOCKER_TAG} --sbom=true --provenance=true -f build.Dockerfile .

.PHONY: build-local build-docker
