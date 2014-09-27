# Package the sources for cl-link-grammar

PACKAGE = cl-link-grammar

default: clean prepare package
	cp -r src tests Makefile README.org ${PACKAGE}.asd ./${PACKAGE}
	find ./${PACKAGE} -regex '.+~$$' -exec rm -rf {} +

prepare:
	mkdir -p ./${PACKAGE}

clean:
	rm -rf ./${PACKAGE}

# This will need to be more specific (uninstall only the version we installed)
uninstall:
	rm -rf /usr/share/common-lisp/source/${PACKAGE}
	find /etc/common-lisp/source-registry.conf.d -regex ".*${PACKAGE}.*" -exec rm -f {} +

install: uninstall
	cp -r ./${PACKAGE} /usr/share/common-lisp/source/${PACKAGE}
	rm -f /usr/share/common-lisp/source/${PACKAGE}/Makefile
	mkdir -p /etc/common-lisp/source-registry.conf.d/
	$(eval LAST_CONF := $(shell ls /etc/common-lisp/source-registry.conf.d/ | tail -n 1))
	$(eval LAST := $(shell echo "${LAST_CONF}" | grep -oP '^[0-9]+'))
	$(eval PREFIX := $(shell echo $(LAST)+1 | bc))
	echo '(:include "/usr/share/common-lisp/source/${PACKAGE}/")' > \
	"/etc/common-lisp/source-registry.conf.d/${PREFIX}-${PACKAGE}.conf"

tar.gz: default
	tar -zcf ${PACKAGE}.tar.gz ./${PACKAGE}

package: tar.gz
