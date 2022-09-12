#!/bin/sh

for EX in examples/*; do
	echo "[INFO] Running: $EX"
	ocaml li.ml $EX || ( echo "[ERROR] Failed!"; exit 1 )
done
