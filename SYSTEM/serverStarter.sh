#!/bin/bash

#while [ "forever" ]
#do
	clisp -on-error exit -E UTF-8 ./parser-service.lisp  .// 
	sleep 1
#done
