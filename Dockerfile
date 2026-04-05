FROM r-base:4.4.2

RUN R -e "install.packages(c('plumber','jsonlite'), repos='https://cloud.r-project.org')"

WORKDIR /app

COPY . .

EXPOSE 10000

CMD ["R", "-e", "pr <- plumber::plumb('plumber.R'); pr$run(host='0.0.0.0', port=10000)"]