.section .data
.align 8
.align 1
.globl __stginit_Main
.type __stginit_Main, @object
__stginit_Main:
.section .rodata
.align 8
.align 1
c1D5_str:
	.byte	109
	.byte	97
	.byte	105
	.byte	110
	.byte	0
.section .data
.align 8
.align 1
r1Cs_closure:
	.quad	ghczmprim_GHCziTypes_TrNameS_static_info
	.quad	c1D5_str
.section .rodata
.align 8
.align 1
c1D9_str:
	.byte	77
	.byte	97
	.byte	105
	.byte	110
	.byte	0
.section .data
.align 8
.align 1
r1CG_closure:
	.quad	ghczmprim_GHCziTypes_TrNameS_static_info
	.quad	c1D9_str
.section .data
.align 8
.align 1
.globl Main_zdtrModule_closure
.type Main_zdtrModule_closure, @object
Main_zdtrModule_closure:
	.quad	ghczmprim_GHCziTypes_Module_static_info
	.quad	r1Cs_closure+1
	.quad	r1CG_closure+1
	.quad	3
.section .data
.align 8
.align 1
s1D3_closure:
	.quad	s1D3_info
	.quad	0
	.quad	0
	.quad	0
.section .text
.align 8
.align 8
	.long	S1En_srt-(s1D1_info)+0
	.long	0
	.quad	0
	.quad	4294967312
s1D1_info:
.Lc1Dt:
	leaq -32(%rbp),%rax
	cmpq %r15,%rax
	jb .Lc1Du
.Lc1Dv:
	addq $16,%r12
	cmpq 856(%r13),%r12
	ja .Lc1Dx
.Lc1Dw:
	movq $stg_upd_frame_info,-16(%rbp)
	movq %rbx,-8(%rbp)
	movq $integerzmgmp_GHCziIntegerziType_Szh_con_info,-8(%r12)
	movq $1,(%r12)
	leaq -7(%r12),%rax
	movl $base_GHCziEnum_zdfEnumInteger_closure,%r14d
	movq $stg_ap_p_info,-32(%rbp)
	movq %rax,-24(%rbp)
	addq $-32,%rbp
	jmp base_GHCziEnum_enumFrom_info
.Lc1Dx:
	movq $16,904(%r13)
.Lc1Du:
	jmp *-16(%r13)
	.size s1D1_info, .-s1D1_info
.section .rodata
.align 8
.align 1
c1DL_str:
	.byte	58
	.byte	32
	.byte	0
.section .text
.align 8
.align 8
	.long	S1En_srt-(s1CP_info)+8
	.long	0
	.quad	0
	.quad	4294967312
s1CP_info:
.Lc1DM:
	leaq -16(%rbp),%rax
	cmpq %r15,%rax
	jb .Lc1DN
.Lc1DO:
	movq $stg_upd_frame_info,-16(%rbp)
	movq %rbx,-8(%rbp)
	movl $c1DL_str,%r14d
	movl $ghczmprim_GHCziCString_unpackCStringzh_closure,%ebx
	addq $-16,%rbp
	jmp stg_ap_n_fast
.Lc1DN:
	jmp *-16(%r13)
	.size s1CP_info, .-s1CP_info
.section .text
.align 8
.align 8
	.long	S1En_srt-(s1CQ_info)+8
	.long	0
	.quad	1
	.quad	12884901905
s1CQ_info:
.Lc1DP:
	leaq -16(%rbp),%rax
	cmpq %r15,%rax
	jb .Lc1DQ
.Lc1DR:
	addq $16,%r12
	cmpq 856(%r13),%r12
	ja .Lc1DT
.Lc1DS:
	movq $stg_upd_frame_info,-16(%rbp)
	movq %rbx,-8(%rbp)
	movq 16(%rbx),%rax
	movq $s1CP_info,-8(%r12)
	leaq -8(%r12),%rbx
	movq %rax,%rsi
	movq %rbx,%r14
	movl $base_GHCziBase_zpzp_closure,%ebx
	addq $-16,%rbp
	jmp stg_ap_pp_fast
.Lc1DT:
	movq $16,904(%r13)
.Lc1DQ:
	jmp *-16(%r13)
	.size s1CQ_info, .-s1CQ_info
.section .text
.align 8
.align 8
	.long	S1En_srt-(s1CO_info)+24
	.long	0
	.quad	1
	.quad	4294967313
s1CO_info:
.Lc1DY:
	leaq -32(%rbp),%rax
	cmpq %r15,%rax
	jb .Lc1DZ
.Lc1E0:
	movq $stg_upd_frame_info,-16(%rbp)
	movq %rbx,-8(%rbp)
	movq 16(%rbx),%rax
	movl $base_GHCziShow_zdfShowInteger_closure,%r14d
	movq $stg_ap_p_info,-32(%rbp)
	movq %rax,-24(%rbp)
	addq $-32,%rbp
	jmp base_GHCziShow_show_info
.Lc1DZ:
	jmp *-16(%r13)
	.size s1CO_info, .-s1CO_info
.section .text
.align 8
.align 8
	.long	S1En_srt-(s1CR_info)+8
	.long	0
	.quad	8589934607
	.quad	4294967296
	.quad	30064771083
s1CR_info:
.Lc1E1:
.Lc1E3:
	addq $48,%r12
	cmpq 856(%r13),%r12
	ja .Lc1E5
.Lc1E4:
	movq $s1CQ_info,-40(%r12)
	movq %rsi,-24(%r12)
	leaq -40(%r12),%rax
	movq $s1CO_info,-16(%r12)
	movq %r14,(%r12)
	leaq -16(%r12),%rbx
	movq %rax,%rsi
	movq %rbx,%r14
	movl $base_GHCziBase_zpzp_closure,%ebx
	jmp stg_ap_pp_fast
.Lc1E5:
	movq $48,904(%r13)
.Lc1E2:
	jmp *-8(%r13)
	.size s1CR_info, .-s1CR_info
.section .text
.align 8
.align 8
	.long	S1En_srt-(s1D2_info)+0
	.long	0
	.quad	0
	.quad	133143986192
s1D2_info:
.Lc1E6:
	leaq -16(%rbp),%rax
	cmpq %r15,%rax
	jb .Lc1E7
.Lc1E8:
	addq $32,%r12
	cmpq 856(%r13),%r12
	ja .Lc1Ea
.Lc1E9:
	movq $stg_upd_frame_info,-16(%rbp)
	movq %rbx,-8(%rbp)
	movq $s1D1_info,-24(%r12)
	leaq -24(%r12),%rax
	movq $s1CR_info,-8(%r12)
	leaq -6(%r12),%rbx
	movq %rax,%rsi
	movq %rbx,%r14
	movl $base_GHCziList_zzipWith_closure,%ebx
	addq $-16,%rbp
	jmp stg_ap_pp_fast
.Lc1Ea:
	movq $32,904(%r13)
.Lc1E7:
	jmp *-16(%r13)
	.size s1D2_info, .-s1D2_info
.section .text
.align 8
.align 8
	.long	S1En_srt-(s1CL_info)+40
	.long	0
	.quad	0
	.quad	64424509456
s1CL_info:
.Lc1Ef:
	leaq -16(%rbp),%rax
	cmpq %r15,%rax
	jb .Lc1Eg
.Lc1Eh:
	movq $stg_upd_frame_info,-16(%rbp)
	movq %rbx,-8(%rbp)
	movl $base_SystemziIO_putStrLn_closure,%edi
	movl $base_GHCziBase_zdfMonadIO_closure,%esi
	movl $base_DataziFoldable_zdfFoldableZMZN_closure,%r14d
	movl $base_DataziFoldable_mapMzu_closure,%ebx
	addq $-16,%rbp
	jmp stg_ap_ppp_fast
.Lc1Eg:
	jmp *-16(%r13)
	.size s1CL_info, .-s1CL_info
.section .text
.align 8
.align 8
	.long	S1En_srt-(s1D3_info)+0
	.long	0
	.quad	0
	.quad	4393751543830
s1D3_info:
.Lc1Ei:
	leaq -16(%rbp),%rax
	cmpq %r15,%rax
	jb .Lc1Ej
.Lc1Ek:
	addq $32,%r12
	cmpq 856(%r13),%r12
	ja .Lc1Em
.Lc1El:
	subq $8,%rsp
	movq %r13,%rax
	movq %rbx,%rsi
	movq %rax,%rdi
	xorl %eax,%eax
	call newCAF
	addq $8,%rsp
	testq %rax,%rax
	je .Lc1Dj
.Lc1Di:
	movq $stg_bh_upd_frame_info,-16(%rbp)
	movq %rax,-8(%rbp)
	movq $s1D2_info,-24(%r12)
	leaq -24(%r12),%rax
	movq $s1CL_info,-8(%r12)
	leaq -8(%r12),%rbx
	movq %rax,%rsi
	movq %rbx,%r14
	movl $base_GHCziBase_zi_closure,%ebx
	addq $-16,%rbp
	jmp stg_ap_pp_fast
.Lc1Dj:
	jmp *(%rbx)
.Lc1Em:
	movq $32,904(%r13)
.Lc1Ej:
	jmp *-16(%r13)
	.size s1D3_info, .-s1D3_info
.section .data
.align 8
.align 1
.globl Main_main_closure
.type Main_main_closure, @object
Main_main_closure:
	.quad	Main_main_info
	.quad	0
	.quad	0
	.quad	0
.section .text
.align 8
.align 8
	.long	S1En_srt-(Main_main_info)+56
	.long	0
	.quad	0
	.quad	107374182422
.globl Main_main_info
.type Main_main_info, @object
Main_main_info:
.Lc1EV:
	leaq -40(%rbp),%rax
	cmpq %r15,%rax
	jb .Lc1EW
.Lc1EX:
	subq $8,%rsp
	movq %r13,%rax
	movq %rbx,%rsi
	movq %rax,%rdi
	xorl %eax,%eax
	call newCAF
	addq $8,%rsp
	testq %rax,%rax
	je .Lc1EU
.Lc1ET:
	movq $stg_bh_upd_frame_info,-16(%rbp)
	movq %rax,-8(%rbp)
	movl $base_GHCziBase_zdfMonadIO_closure,%r14d
	movq $stg_ap_pp_info,-40(%rbp)
	movq $base_SystemziEnvironment_getArgs_closure,-32(%rbp)
	movq $s1D3_closure,-24(%rbp)
	addq $-40,%rbp
	jmp base_GHCziBase_zgzgze_info
.Lc1EU:
	jmp *(%rbx)
.Lc1EW:
	jmp *-16(%r13)
	.size Main_main_info, .-Main_main_info
.section .data
.align 8
.align 1
.globl ZCMain_main_closure
.type ZCMain_main_closure, @object
ZCMain_main_closure:
	.quad	ZCMain_main_info
	.quad	0
	.quad	0
	.quad	0
.section .text
.align 8
.align 8
	.long	S1En_srt-(ZCMain_main_info)+96
	.long	0
	.quad	0
	.quad	12884901910
.globl ZCMain_main_info
.type ZCMain_main_info, @object
ZCMain_main_info:
.Lc1Fa:
	leaq -16(%rbp),%rax
	cmpq %r15,%rax
	jb .Lc1Fb
.Lc1Fc:
	subq $8,%rsp
	movq %r13,%rax
	movq %rbx,%rsi
	movq %rax,%rdi
	xorl %eax,%eax
	call newCAF
	addq $8,%rsp
	testq %rax,%rax
	je .Lc1F9
.Lc1F8:
	movq $stg_bh_upd_frame_info,-16(%rbp)
	movq %rax,-8(%rbp)
	movl $Main_main_closure,%r14d
	movl $base_GHCziTopHandler_runMainIO_closure,%ebx
	addq $-16,%rbp
	jmp stg_ap_p_fast
.Lc1F9:
	jmp *(%rbx)
.Lc1Fb:
	jmp *-16(%r13)
	.size ZCMain_main_info, .-ZCMain_main_info
.section .data.rel.ro
.align 8
.align 1
S1En_srt:
	.quad	base_GHCziEnum_zdfEnumInteger_closure
	.quad	ghczmprim_GHCziCString_unpackCStringzh_closure
	.quad	base_GHCziBase_zpzp_closure
	.quad	base_GHCziShow_zdfShowInteger_closure
	.quad	base_GHCziList_zzipWith_closure
	.quad	base_SystemziIO_putStrLn_closure
	.quad	base_DataziFoldable_mapMzu_closure
	.quad	base_GHCziBase_zdfMonadIO_closure
	.quad	base_DataziFoldable_zdfFoldableZMZN_closure
	.quad	base_GHCziBase_zi_closure
	.quad	base_SystemziEnvironment_getArgs_closure
	.quad	s1D3_closure
	.quad	base_GHCziTopHandler_runMainIO_closure
	.quad	Main_main_closure
.section .note.GNU-stack,"",@progbits
.ident "GHC 8.0.2"

