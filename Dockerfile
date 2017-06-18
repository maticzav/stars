FROM node:7.2.0

MAINTAINER Matic Zavadlal <matic.zavadlal@gmail.com>

# Install elm
RUN npm install -g elm@0.18.0
RUN npm install -g elm-test@0.18.0

# Create app directory
RUN mkdir /app/
WORKDIR /app

# Install elm modules
COPY elm-package.json /app/
RUN elm package install -y

# Install npm dependencies
COPY package.json /app/
RUN npm install

# Bundle source
COPY . /app

# Build
RUN elm make Main.elm --output build/index.js

# Log files
RUN ls build/

# Start
EXPOSE 4000
ENV PORT=4000
ENTRYPOINT ["npm","run","start"]
