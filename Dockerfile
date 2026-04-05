FROM r-base:4.4.2

# Install system dependencies (VERY IMPORTANT 🔥)
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libsodium-dev \
    libxml2-dev \
    make \
    g++ \
    && rm -rf /var/lib/apt/lists/*

# Install R packages
RUN R -e "install.packages(c('plumber','jsonlite'), repos='https://cloud.r-project.org')"

# Set working directory
WORKDIR /app

# Copy files
COPY . .

# Expose port
EXPOSE 10000

# Run API
CMD ["R", "-e", "pr <- plumber::plumb('plumber.R'); pr$run(host='0.0.0.0', port=10000)"]
