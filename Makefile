# Package the sources for cl-link-grammar

VERSION = 0.0.0
PACKAGE = cl-link-grammar

default: clean prepare package

prepare:
	mkdir -p ./${PACKAGE}-${VERSION}

clean:
	rm -rf ./${PACKAGE}-${VERSION}
	rm -rf ./SOURCES/*

# This will need to be more specific (uninstall only the version we installed)
uninstall:
	rm -rf /usr/share/common-lisp/source/${PACKAGE}
	find /etc/common-lisp/source-registry.conf.d -regex ".*${PACKAGE}.*" -exec rm -f {} +

install: uninstall
	cp -r ./${PACKAGE}-${VERSION} /usr/share/common-lisp/source/${PACKAGE}
	rm -f /usr/share/common-lisp/source/${PACKAGE}/Makefile
	mkdir -p /etc/common-lisp/source-registry.conf.d/
	$(eval LAST_CONF := $(shell ls /etc/common-lisp/source-registry.conf.d/ | tail -n 1))
	$(eval LAST := $(shell echo "${LAST_CONF}" | grep -oP '^[0-9]+'))
	$(eval PREFIX := $(shell echo $(LAST)+1 | bc))
	echo '(:include "/usr/share/common-lisp/source/${PACKAGE}/")' > \
	"/etc/common-lisp/source-registry.conf.d/${PREFIX}-${PACKAGE}.conf"

tar.gz:
	cp -r src tests Makefile README.org ${PACKAGE}.asd ./${PACKAGE}-${VERSION}
	find ./${PACKAGE}-${VERSION} -regex '.+~$$' -exec rm -rf {} +
	tar -zcf ${PACKAGE}-${VERSION}.tar.gz ./${PACKAGE}-${VERSION}/*
	mv ./${PACKAGE}-${VERSION}.tar.gz ./SOURCES

package: tar.gz
