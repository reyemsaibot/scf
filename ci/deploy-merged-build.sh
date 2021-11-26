#!/bin/bash

# Build merged
npm run merge || exit 1
wc -l ./zap_scf.abap

# Deploy artifacts
git clone https://github.com/reyemsaibot/scf.git
cp zap_scf.abap scf/last_build/zap_scf_standalone.prog.abap
cd scf

# Commit
git status
git config user.email "ci@loona.org"
git config user.name "CI"
git add last_build/zap_scf.prog.abap
git commit -m "CI build [skip ci]" || exit 1
git push -q https://$GITHUB_API_KEY@github.com/reyemsaibot/scf.git 
