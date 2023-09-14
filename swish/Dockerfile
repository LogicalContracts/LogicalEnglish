FROM cclawdev/swish:master as base

RUN swipl -t 'pack_install(date_time, [interactive(false)]).'

COPY swish/config-enabled /data/config-enabled
COPY . /app
