ifndef QUARTUS_ROOTDIR
  ifneq ($(MAKECMDGOALS), clean)
    $(error Please set QUARTUS_ROOTDIR)
  endif
endif

.PHONY: all
all: 
	qsys-generate -syn S5_DDR3_QSYS.qsys
	quartus_dse Golden_top.qpf    \
    --num-seeds 15              \
    --launcher local            \
    --num-concurrent 4
	quartus_dse Golden_top.qpf --report utilization
	quartus_dse Golden_top.qpf --report fmax_summary

.PHONY: one
one: 
	qsys-generate -syn S5_DDR3_QSYS.qsys
	quartus_sh --flow compile Golden_top.qpf

.PHONY: report
report:
	quartus_dse Golden_top.qpf --report utilization
	quartus_dse Golden_top.qpf --report fmax_summary

.PHONY: update-mif
update-mif:
	make -C software
	quartus_cdb --update_mif Golden_top.qpf
	quartus_asm Golden_top.qpf

.PHONY: download-sof
download-sof:
	quartus_pgm -m jtag -o "p;Golden_top.sof"

.PHONY: clean
clean:
	rm -f *.mif *.rpt *.msg *.summary *.sld *.sopcinfo *.jdi
	rm -f *.pin *.done *.qws *.sof *.csv *.qws *.smsg
	rm -rf dse* db incremental_db S5_DDR3_QSYS .qsys_edit reconfig_mif
	make -C software clean
