FROM fredhutch/r-shiny-base:4.6.0

RUN apt-get update -y && apt-get install -y curl nginx cmake libpq-dev libuv1-dev

RUN curl -LO https://github.com/quarto-dev/quarto-cli/releases/download/v1.10.18/quarto-1.10.18-linux-amd64.deb

RUN dpkg -i quarto-1.10.18-linux-amd64.deb && rm quarto-1.10.18-linux-amd64.deb

ADD check.R /tmp/

RUN R -e 'install.packages(c("renv"))'

COPY sota2026/renv.lock /apps/sota2026/renv.lock

WORKDIR /apps/sota2026

RUN R -e 'renv::restore(prompt = FALSE)'

WORKDIR /apps

# TODO, install subsequent apps

RUN R -f /tmp/check.R --args renv

RUN rm -f /etc/nginx/sites-enabled/default /etc/nginx/conf.d/default.conf /etc/nginx/sites-available/default
COPY ./nginx.conf /etc/nginx/nginx.conf
COPY ./index.html /var/www/html/index.html
COPY ./index.html /usr/share/nginx/html/index.html
RUN rm /var/www/html/index.nginx-debian.html

ADD . /apps/


EXPOSE 80

ENTRYPOINT ["/apps/start.sh"]

