FROM alpine:latest as scala-cli-installer

RUN apk add --no-cache bash curl ca-certificates gzip gcompat

WORKDIR /app

RUN curl -fL https://github.com/coursier/coursier/releases/latest/download/cs-x86_64-pc-linux.gz | gzip -d > cs && \
    chmod +x cs && \
    ./cs setup --yes --apps scala-cli --jvm 11 && \
    rm cs

ENV PATH="/root/.local/share/coursier/bin:$PATH"

FROM scala-cli-installer as scala-app-builder

COPY src /app

RUN scala-cli --power package . -o app --assembly

FROM eclipse-temurin:17-jre-alpine
RUN apk add --no-cache bash
COPY --from=scala-app-builder /app/app /app/src/app
COPY --from=scala-app-builder /app/data /app/src/data
WORKDIR /app
CMD ["./src/app"]
