// File generated by Gambit v4.9.4
// Link info: (409004 (js ((compactness 9))) "srfi/2/syntax-utils" (("srfi/2/syntax-utils")) (module_register entrypt_init returnpt_init ctrlpt_init parententrypt_init r2 check_procedure r4 closure_alloc check_procedure_glo poll r1 r0 sp stack make_interned_symbol glo wrong_nargs nargs) (cadr datum->syntax ##source? car ##source-code srfi/2/syntax-utils#syntax-car srfi/2/syntax-utils#syntax-cadr srfi/2/syntax-utils#syntax-lift) (srfi/2/syntax-utils# srfi/2/syntax-utils#syntax-car srfi/2/syntax-utils#syntax-cadr srfi/2/syntax-utils#syntax-lift) (srfi/2/syntax-utils#syntax-car srfi/2/syntax-utils#syntax-cadr srfi/2/syntax-utils#syntax-lift) #f)

_cst0__srfi_2f_2_2f_syntax_2d_utils = _i("srfi/2/syntax-utils#syntax-lift");

_cst1__srfi_2f_2_2f_syntax_2d_utils = _i("srfi/2/syntax-utils#syntax-car");

_cst2__srfi_2f_2_2f_syntax_2d_utils = _i("srfi/2/syntax-utils#syntax-cadr");

_cst3__srfi_2f_2_2f_syntax_2d_utils = _i("##source-code");

_cst4__srfi_2f_2_2f_syntax_2d_utils = _i("##source?");

_cst5__srfi_2f_2_2f_syntax_2d_utils = _i("datum->syntax");

_m(_bb1_srfi_2f_2_2f_syntax_2d_utils_23_ = () => {
if (_n !== 0) {
return _w(_bb1_srfi_2f_2_2f_syntax_2d_utils_23_);
}
_g["srfi/2/syntax-utils#syntax-lift"] = _bb1_srfi_2f_2_2f_syntax_2d_utils_23_syntax_2d_lift;
_s[_t+1] = _r;
_a = _g["car"];
++_t;
return _p(_bb2_srfi_2f_2_2f_syntax_2d_utils_23_);
},-1,_i("srfi/2/syntax-utils#"),!1,!0);

_j(_bb2_srfi_2f_2_2f_syntax_2d_utils_23_ = () => {
_r = _bb3_srfi_2f_2_2f_syntax_2d_utils_23_;
_n = 1;
return _u(_g["srfi/2/syntax-utils#syntax-lift"],_cst0__srfi_2f_2_2f_syntax_2d_utils)();
});

_k(_bb3_srfi_2f_2_2f_syntax_2d_utils_23_ = () => {
_g["srfi/2/syntax-utils#syntax-car"] = _a;
_a = _g["cadr"];
_r = _bb4_srfi_2f_2_2f_syntax_2d_utils_23_;
_n = 1;
return _u(_g["srfi/2/syntax-utils#syntax-lift"],_cst0__srfi_2f_2_2f_syntax_2d_utils)();
},1,1);

_k(_bb4_srfi_2f_2_2f_syntax_2d_utils_23_ = () => {
_g["srfi/2/syntax-utils#syntax-cadr"] = _a;
_a = void 0;
--_t;
return _s[_t+1]();
},1,1);



_m(_bb1_srfi_2f_2_2f_syntax_2d_utils_23_syntax_2d_lift = () => {
if (_n !== 1) {
return _w(_bb1_srfi_2f_2_2f_syntax_2d_utils_23_syntax_2d_lift);
}
return _bb2_srfi_2f_2_2f_syntax_2d_utils_23_syntax_2d_lift();
},-1,_cst0__srfi_2f_2_2f_syntax_2d_utils,!1,!1);

_j(_bb2_srfi_2f_2_2f_syntax_2d_utils_23_syntax_2d_lift = () => {
var closure0 = _closure_alloc([_bb3_srfi_2f_2_2f_syntax_2d_utils_23_syntax_2d_lift,_a]);
_s[_t+1] = closure0;
_a = _s[_t+1];
return _r;
});

_l(_bb3_srfi_2f_2_2f_syntax_2d_utils_23_syntax_2d_lift = () => {
if (_n !== 1) {
return _w(_d);
}
return _bb4_srfi_2f_2_2f_syntax_2d_utils_23_syntax_2d_lift();
},1);

_j(_bb4_srfi_2f_2_2f_syntax_2d_utils_23_syntax_2d_lift = () => {
_s[_t+1] = _r;
_s[_t+2] = _a;
_s[_t+3] = _d;
_t += 3;
return _p(_bb7_srfi_2f_2_2f_syntax_2d_utils_23_syntax_2d_lift);
});

_j(_bb7_srfi_2f_2_2f_syntax_2d_utils_23_syntax_2d_lift = () => {
_r = _bb6_srfi_2f_2_2f_syntax_2d_utils_23_syntax_2d_lift;
_n = 1;
return _u(_g["##source-code"],_cst3__srfi_2f_2_2f_syntax_2d_utils)();
});

_k(_bb6_srfi_2f_2_2f_syntax_2d_utils_23_syntax_2d_lift = () => {
_r = _bb5_srfi_2f_2_2f_syntax_2d_utils_23_syntax_2d_lift;
_n = 1;
--_t;
return _check_procedure(_s[_t+1](!0)[1])();
},3,1);

_k(_bb5_srfi_2f_2_2f_syntax_2d_utils_23_syntax_2d_lift = () => {
_s[_t+1] = _a;
_r = _bb8_srfi_2f_2_2f_syntax_2d_utils_23_syntax_2d_lift;
_n = 1;
++_t;
return _u(_g["##source?"],_cst4__srfi_2f_2_2f_syntax_2d_utils)();
},2,1);

_k(_bb8_srfi_2f_2_2f_syntax_2d_utils_23_syntax_2d_lift = () => {
if (!(_a === !1)) {
return _bb9_srfi_2f_2_2f_syntax_2d_utils_23_syntax_2d_lift();
} else {
return _bb10_srfi_2f_2_2f_syntax_2d_utils_23_syntax_2d_lift();
}
},3,1);

_j(_bb9_srfi_2f_2_2f_syntax_2d_utils_23_syntax_2d_lift = () => {
_a = _s[_t];
_t -= 3;
return _s[_t+1]();
});

_j(_bb10_srfi_2f_2_2f_syntax_2d_utils_23_syntax_2d_lift = () => {
_b = _s[_t];
_a = _s[_t-1];
_r = _s[_t-2];
return _p(_bb11_srfi_2f_2_2f_syntax_2d_utils_23_syntax_2d_lift);
});

_j(_bb11_srfi_2f_2_2f_syntax_2d_utils_23_syntax_2d_lift = () => {
_n = 2;
_t -= 3;
return _u(_g["datum->syntax"],_cst5__srfi_2f_2_2f_syntax_2d_utils)();
});



_module_register([[_i("srfi/2/syntax-utils")],[_i("gambit")],null,1,_bb1_srfi_2f_2_2f_syntax_2d_utils_23_,!1]);

