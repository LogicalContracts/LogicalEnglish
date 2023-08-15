#!/bin/bash

if [ $# -lt 1 ]; then
  echo "Usage: $0 version"
  exit 1
fi

PACK_VERSION="${@:1}"

PACK_DIR="./pack/logicalenglish"
cp pack.pl LICENSE.txt README.md $PACK_DIR

PACK_CODE_DIR="./pack/logicalenglish/prolog/"
PACK_CODE_DIR_TOKENIZER="$PACK_CODE_DIR/tokenize/prolog/"
PACK_CODE_DIR_SPACY="$PACK_CODE_DIR/spacy/"

mkdir -p $PACK_CODE_DIR
mkdir -p $PACK_CODE_DIR_TOKENIZER
mkdir -p $PACK_CODE_DIR_SPACY

cp api.pl drafter.pl kp_loader.pl le_answer.pl le_input.pl le_local.pl reasoner.pl syntax.pl $PACK_CODE_DIR
cp ./tokenize/prolog/tokenize.pl $PACK_CODE_DIR_TOKENIZER
cp ./tokenize/prolog/tokenize_opts.pl $PACK_CODE_DIR_TOKENIZER
cp ./spacy/spacy.pl $PACK_CODE_DIR_SPACY

cd ./pack

zip -r logicalenglish-$PACK_VERSION.zip logicalenglish

# to be used inside Prolog with ?- pack_install('logicalenglish-<...>.zip').