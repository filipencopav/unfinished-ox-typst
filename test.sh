#!/bin/sh

typst compile org-export-better-documentation.typ && zathura org-export-better-documentation.pdf
