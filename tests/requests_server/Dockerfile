FROM golang:1.18.2-bullseye AS build
WORKDIR /build
COPY go.mod ./
COPY go.sum ./
RUN go mod download
COPY *.go ./
RUN GOOS=linux go build -a -o /app.exe

FROM gcr.io/distroless/base:latest
WORKDIR /
COPY --from=build /app.exe /app.exe
ENV GIN_MODE release
USER nonroot:nonroot
ENTRYPOINT [ "/app.exe" ]
