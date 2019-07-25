ifndef QUARTUS_ROOTDIR
  ifneq ($(MAKECMDGOALS), clean)
    $(error Please set QUARTUS_ROOTDIR)
  endif
endif

.PHONY: all
all: qsys
	quartus_dse top.qpf    \
    --num-seeds 15              \
    --launcher local            \
    --num-concurrent 4
	quartus_dse top.qpf --report utilization
	quartus_dse top.qpf --report fmax_summary

.PHONY: qsys
qsys:
	qsys-generate -syn soc.qsys --quartus-project=top.qpf

.PHONY: one
one: qsys
	quartus_sh --flow compile top.qpf

.PHONY: report
report:
	quartus_dse top.qpf --report utilization
	quartus_dse top.qpf --report fmax_summary

.PHONY: update-mif
update-mif:
	make -C software
	quartus_cdb --update_mif top.qpf
	quartus_asm top.qpf

.PHONY: download-sof
download-sof:
	quartus_pgm -m jtag -o "p;output_files/top.sof"

.PHONY: clean
clean:
	rm -rf tmp-clearbox synth_dumps qdb output_files soc
	rm -f top.qws *.rpt soc.tcl
	ls ip/soc/ | grep -v -E '\.ip$$' | xargs -i rm -rf ip/soc/{}
	make -C software clean
