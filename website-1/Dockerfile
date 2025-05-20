FROM ocaml/opam:alpine

USER root

RUN apk add --no-cache \
    gcc \
    dune \
    ocaml \
    pkgconfig \
    openssl-dev \
    npm \
    nodejs \
    libev-dev

USER opam

RUN opam install dune dream dream-html ocamlfind

COPY --chown=opam . /app
WORKDIR /app

RUN npm install tailwindcss
RUN npx tailwindcss -i static/input.css -o static/output.css

RUN eval $(opam env)

RUN opam exec -- dune build
EXPOSE 2048/tcp
CMD ["dune", "exec", "bin/main.exe"]
