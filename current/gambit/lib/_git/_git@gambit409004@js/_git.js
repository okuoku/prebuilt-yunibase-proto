// File generated by Gambit v4.9.4
// Link info: (409004 (js ((compactness 9))) "_git" (("_git")) (module_register entrypt_init returnpt_init ctrlpt_init parententrypt_init eof_obj closure_alloc jsnumberp peps pairp r4 absent_obj cons make_interned_keyword structure string base92_decode make_interned_symbol glo r2 r3 poll sp stack stringp r0 r1 wrong_nargs nargs) (##fail-check-procedure ##os-environ ##read-line ##process-status ##path-expand ##file-info ##fail-check-string ##write-string ##file-exists? ##fail-check-string-list ##file-info-type _git#d ##repl ##tty-mode-reset _tar#tar-unpack-port ##call-with-input-process) (_git#git-repository-open _git#git-command-aux _git#git-pull _git#git-command _git#git-tag _git#git-archive _git# _git#git-clone _git#git-status) () #f)

_cst0____git = _i("##file-exists?");

_cst1____git = _i("##fail-check-string");

_cst2____git = _i("##file-info");

_cst3____git = _i("##file-info-type");

_cst4____git = _i("directory");

_cst5____git = new _ScmString(_z("QcJcLcW"));

_cst6____git = _i("##path-expand");

_cst17____git = _i("##type-1-AF9B3B94-EE56-4D95-A323-AEE3C97E70FC");

_cst7____git = new _Structure([new _Structure([null,_i("##type-5"),_i("type"),8,!1,[_i("id"),1,!1,_i("name"),5,!1,_i("flags"),5,!1,_i("super"),5,!1,_i("fields"),5,!1]]),_cst17____git,_i("git-repository"),29,!1,[_i("path"),18,!1]]);

_cst21____git = _i("##tty-mode-reset");

_cst22____git = _i("##os-environ");

_cst23____git = _make_interned_keyword("path");

_cst24____git = new _ScmString(_z("cJcLcW"));

_cst25____git = _make_interned_keyword("arguments");

_cst26____git = _make_interned_keyword("directory");

_cst27____git = _make_interned_keyword("stdin-redirection");

_cst28____git = _make_interned_keyword("stdout-redirection");

_cst29____git = _make_interned_keyword("stderr-redirection");

_cst30____git = _make_interned_keyword("pseudo-terminal");

_cst31____git = _make_interned_keyword("environment");

_cst32____git = new _ScmString(_z("c*c,c7cBc7c(c5c0c,c1c$c/cBc3c5c2c0c3c7`S"));

_cst33____git = _i("##call-with-input-process");

_cst34____git = _i("_git#d");

_cst35____git = _i("##reverse");

_cst36____git = _i("##fail-check-string-list");

_cst37____git = _i("##fail-check-procedure");

_cst38____git = new _ScmString(_z("cDcUcFcKcLcYcH"));

_cst39____git = _i("##process-status");

_cst40____git = _i("_tar#tar-unpack-port");

_cst41____git = _i("##repl");

_cst42____git = new _ScmString(_z("cFcOcRcQcH"));

_cst43____git = _i("##newline1");

_cst44____git = _i("##read-line");

_cst45____git = _i("##write-string");

_cst46____git = new _ScmString(_z("cScXcOcO"));

_cst47____git = new _ScmString(_z("cRcUcLcJcLcQ"));

_cst48____git = new _ScmString(_z("cVcWcDcWcXcV"));

_cst49____git = new _ScmString(_z("cWcDcJ"));

_m(_bb1___git_23_ = () => {
if (_n !== 0) {
return _w(_bb1___git_23_);
}
_a = void 0;
return _r;
},-1,_i("_git#"),!1,!0);



_m(_bb1___git_23_git_2d_repository_2d_open = () => {
if (_n !== 1) {
return _w(_bb1___git_23_git_2d_repository_2d_open);
}
if (_stringp(_a)) {
return _bb2___git_23_git_2d_repository_2d_open();
} else {
return _bb19___git_23_git_2d_repository_2d_open();
}
},-1,_i("_git#git-repository-open"),!1,!0);

_j(_bb2___git_23_git_2d_repository_2d_open = () => {
_s[_t+1] = _r;
_s[_t+2] = _a;
_t += 2;
return _p(_bb3___git_23_git_2d_repository_2d_open);
});

_j(_bb19___git_23_git_2d_repository_2d_open = () => {
_c = _a;
_b = _bb1___git_23_git_2d_repository_2d_open;
_a = 1;
return _p(_bb20___git_23_git_2d_repository_2d_open);
});

_j(_bb3___git_23_git_2d_repository_2d_open = () => {
_r = _bb4___git_23_git_2d_repository_2d_open;
_n = 1;
return _g["##file-exists?"]();
});

_j(_bb20___git_23_git_2d_repository_2d_open = () => {
_n = 3;
return _g["##fail-check-string"]();
});

_k(_bb4___git_23_git_2d_repository_2d_open = () => {
if (!(_a === !1)) {
return _bb5___git_23_git_2d_repository_2d_open();
} else {
return _bb15___git_23_git_2d_repository_2d_open();
}
},2,1);

_j(_bb5___git_23_git_2d_repository_2d_open = () => {
_a = _s[_t];
_r = _bb6___git_23_git_2d_repository_2d_open;
_n = 1;
return _g["##file-info"]();
});

_j(_bb15___git_23_git_2d_repository_2d_open = () => {
_t -= 2;
return _s[_t+1]();
});

_k(_bb6___git_23_git_2d_repository_2d_open = () => {
_r = _bb7___git_23_git_2d_repository_2d_open;
_n = 1;
return _g["##file-info-type"]();
},2,1);

_k(_bb7___git_23_git_2d_repository_2d_open = () => {
if (_cst4____git === _a) {
return _bb8___git_23_git_2d_repository_2d_open();
} else {
return _bb18___git_23_git_2d_repository_2d_open();
}
},2,1);

_j(_bb8___git_23_git_2d_repository_2d_open = () => {
_b = _s[_t];
_a = _cst5____git;
_r = _bb9___git_23_git_2d_repository_2d_open;
_n = 2;
return _g["##path-expand"]();
});

_j(_bb18___git_23_git_2d_repository_2d_open = () => {
_a = !1;
_t -= 2;
return _s[_t+1]();
});

_k(_bb9___git_23_git_2d_repository_2d_open = () => {
_s[_t+1] = _a;
_r = _bb10___git_23_git_2d_repository_2d_open;
_n = 1;
++_t;
return _g["##file-exists?"]();
},2,1);

_k(_bb10___git_23_git_2d_repository_2d_open = () => {
if (!(_a === !1)) {
return _bb16___git_23_git_2d_repository_2d_open();
} else {
return _bb11___git_23_git_2d_repository_2d_open();
}
},3,1);

_j(_bb16___git_23_git_2d_repository_2d_open = () => {
_a = _s[_t];
_r = _bb17___git_23_git_2d_repository_2d_open;
_n = 1;
--_t;
return _g["##file-info"]();
});

_j(_bb11___git_23_git_2d_repository_2d_open = () => {
if (!(_a === !1)) {
--_t;
return _bb13___git_23_git_2d_repository_2d_open();
} else {
--_t;
return _bb15___git_23_git_2d_repository_2d_open();
}
});

_k(_bb17___git_23_git_2d_repository_2d_open = () => {
_r = _bb12___git_23_git_2d_repository_2d_open;
_n = 1;
return _g["##file-info-type"]();
},2,1);

_j(_bb13___git_23_git_2d_repository_2d_open = () => {
_a = _s[_t];
_r = _bb14___git_23_git_2d_repository_2d_open;
_n = 1;
--_t;
return _g["##path-expand"]();
});

_k(_bb12___git_23_git_2d_repository_2d_open = () => {
_a = _cst4____git === _a;
if (!(_a === !1)) {
return _bb13___git_23_git_2d_repository_2d_open();
} else {
return _bb15___git_23_git_2d_repository_2d_open();
}
},2,1);

_k(_bb14___git_23_git_2d_repository_2d_open = () => {
_a = new _Structure([_cst7____git,_a]);
--_t;
return _s[_t+1]();
},1,1);



_m(_bb1___git_23_git_2d_command_2d_aux = () => {
if (_n !== 4) {
return _w(_bb1___git_23_git_2d_command_2d_aux);
}
if (_b instanceof _Structure && _b.a[0].a[1] === _cst17____git) {
return _bb2___git_23_git_2d_command_2d_aux();
} else {
return _bb3___git_23_git_2d_command_2d_aux();
}
},-1,_i("_git#git-command-aux"),!1,!0);

_j(_bb2___git_23_git_2d_command_2d_aux = () => {
_b = _b.a[1];
_s[_t+1] = _a;
_a = _b;
++_t;
return _bb4___git_23_git_2d_command_2d_aux();
});

_j(_bb3___git_23_git_2d_command_2d_aux = () => {
_s[_t+1] = _a;
_a = !1;
++_t;
return _bb4___git_23_git_2d_command_2d_aux();
});

_j(_bb4___git_23_git_2d_command_2d_aux = () => {
_s[_t+1] = _r;
_s[_t+2] = _a;
_s[_t+3] = _c;
_t += 3;
return _p(_bb5___git_23_git_2d_command_2d_aux);
});

_j(_bb5___git_23_git_2d_command_2d_aux = () => {
_r = _bb6___git_23_git_2d_command_2d_aux;
_n = 0;
return _g["##tty-mode-reset"]();
});

_k(_bb6___git_23_git_2d_command_2d_aux = () => {
if (!(_s[_t] === !1)) {
return _bb7___git_23_git_2d_command_2d_aux();
} else {
return _bb11___git_23_git_2d_command_2d_aux();
}
},5,3);

_j(_bb7___git_23_git_2d_command_2d_aux = () => {
_a = !1;
return _bb9___git_23_git_2d_command_2d_aux();
});

_j(_bb11___git_23_git_2d_command_2d_aux = () => {
_r = _bb8___git_23_git_2d_command_2d_aux;
_n = 0;
return _g["##os-environ"]();
});

_j(_bb9___git_23_git_2d_command_2d_aux = () => {
_b = _s[_t] === !1;
_c = _s[_t] === !1;
_a = _X(_cst23____git,_X(_cst24____git,_X(_cst25____git,_X(_s[_t-4],_X(_cst26____git,_X(_s[_t-1],_X(_cst27____git,_X(!1,_X(_cst28____git,_X(_c,_X(_cst29____git,_X(_b,_X(_cst30____git,_X(_s[_t],_X(_cst31____git,_X(_a,null))))))))))))))));
_b = _s[_t-3];
_r = _s[_t-2];
return _p(_bb10___git_23_git_2d_command_2d_aux);
});

_k(_bb8___git_23_git_2d_command_2d_aux = () => {
_a = _X(_cst32____git,_a);
return _bb9___git_23_git_2d_command_2d_aux();
},5,3);

_j(_bb10___git_23_git_2d_command_2d_aux = () => {
_n = 2;
_t -= 5;
return _g["##call-with-input-process"]();
});



_m(_bb1___git_23_git_2d_command = () => {
if (_n === 2) {
_s[++_t] = _a;
_a = _b;
_b = _absent_obj;
_c = _absent_obj;
} else {
if (_n === 3) {
_s[++_t] = _a;
_a = _b;
_b = _c;
_c = _absent_obj;
} else {
if (_n !== 4) {
return _w(_bb1___git_23_git_2d_command);
}
}
}
if (_c === _absent_obj) {
return _bb2___git_23_git_2d_command();
} else {
return _bb3___git_23_git_2d_command();
}
},-1,_i("_git#git-command"),!1,!0);

_j(_bb2___git_23_git_2d_command = () => {
_d = !1;
return _bb4___git_23_git_2d_command();
});

_j(_bb3___git_23_git_2d_command = () => {
_d = _c;
return _bb4___git_23_git_2d_command();
});

_j(_bb4___git_23_git_2d_command = () => {
_s[_t+1] = _a;
_s[_t+2] = _b;
_s[_t+3] = _c;
_b = _s[_t];
_a = _d;
_c = null;
_t += 3;
return _p(_bb5___git_23_git_2d_command);
});

_j(_bb5___git_23_git_2d_command = () => {
if (_x(_b)) {
return _bb6___git_23_git_2d_command();
} else {
return _bb10___git_23_git_2d_command();
}
});

_j(_bb6___git_23_git_2d_command = () => {
_d = _b.a;
if (_stringp(_d)) {
return _bb7___git_23_git_2d_command();
} else {
return _bb8___git_23_git_2d_command();
}
});

_j(_bb10___git_23_git_2d_command = () => {
if (typeof _s[_t-2] === "function" && !Object.prototype.hasOwnProperty.call(_s[_t-2],"h")) {
return _bb13___git_23_git_2d_command();
} else {
return _bb11___git_23_git_2d_command();
}
});

_j(_bb7___git_23_git_2d_command = () => {
_b = _b.b;
_c = _X(_d,_c);
return _p(_bb5___git_23_git_2d_command);
});

_j(_bb8___git_23_git_2d_command = () => {
_s[_t-1] = _s[_t-3];
_s[_t-3] = 1;
_s[_t+1] = _s[_t-2];
_s[_t-2] = _bb1___git_23_git_2d_command;
_c = _s[_t];
_a = _s[_t+1];
_b = _g["_git#d"];
++_t;
return _p(_bb9___git_23_git_2d_command);
});

_j(_bb13___git_23_git_2d_command = () => {
_s[_t-3] = _r;
_s[_t] = _a;
_a = _c;
_r = _bb14___git_23_git_2d_command;
_n = 1;
return _e["##reverse"]();
});

_j(_bb11___git_23_git_2d_command = () => {
_s[_t+1] = _s[_t-3];
_s[_t-3] = 2;
_s[_t+2] = _s[_t-2];
_s[_t-2] = _bb1___git_23_git_2d_command;
_s[_t+3] = _s[_t-1];
_s[_t-1] = _s[_t+1];
_c = _s[_t];
_b = _s[_t+3];
_a = _s[_t+2];
_t += 3;
return _p(_bb12___git_23_git_2d_command);
});

_j(_bb9___git_23_git_2d_command = () => {
_n = 6;
_t -= 2;
return _g["##fail-check-string-list"]();
});

_k(_bb14___git_23_git_2d_command = () => {
_s[_t+1] = _s[_t-3];
_s[_t-3] = _a;
_c = _s[_t];
_b = _s[_t-1];
_a = _s[_t-2];
_r = _s[_t+1];
++_t;
return _p(_bb15___git_23_git_2d_command);
},4,1);

_j(_bb12___git_23_git_2d_command = () => {
_n = 6;
_t -= 4;
return _g["##fail-check-procedure"]();
});

_j(_bb15___git_23_git_2d_command = () => {
_n = 4;
_t -= 4;
return _bb1___git_23_git_2d_command_2d_aux();
});



_m(_bb1___git_23_git_2d_archive = () => {
if (_n !== 2) {
return _w(_bb1___git_23_git_2d_archive);
}
if (_a instanceof _Structure && _a.a[0].a[1] === _cst17____git) {
return _bb2___git_23_git_2d_archive();
} else {
return _bb19___git_23_git_2d_archive();
}
},-1,_i("_git#git-archive"),!1,!0);

_j(_bb2___git_23_git_2d_archive = () => {
_b = _X(_cst38____git,_X(_b,null));
if (_a instanceof _Structure && _a.a[0].a[1] === _cst17____git) {
return _bb3___git_23_git_2d_archive();
} else {
return _bb4___git_23_git_2d_archive();
}
});

_j(_bb19___git_23_git_2d_archive = () => {
_a = !1;
return _r;
});

_j(_bb3___git_23_git_2d_archive = () => {
_a = _a.a[1];
return _bb5___git_23_git_2d_archive();
});

_j(_bb4___git_23_git_2d_archive = () => {
_a = !1;
return _bb5___git_23_git_2d_archive();
});

_j(_bb5___git_23_git_2d_archive = () => {
_s[_t+1] = _r;
_s[_t+2] = _a;
_s[_t+3] = _b;
_t += 3;
return _p(_bb6___git_23_git_2d_archive);
});

_j(_bb6___git_23_git_2d_archive = () => {
_r = _bb7___git_23_git_2d_archive;
_n = 0;
return _g["##tty-mode-reset"]();
});

_k(_bb7___git_23_git_2d_archive = () => {
_r = _bb8___git_23_git_2d_archive;
_n = 0;
return _g["##os-environ"]();
},3,1);

_k(_bb8___git_23_git_2d_archive = () => {
_a = _X(_cst32____git,_a);
_a = _X(_cst23____git,_X(_cst24____git,_X(_cst25____git,_X(_s[_t],_X(_cst26____git,_X(_s[_t-1],_X(_cst27____git,_X(!1,_X(_cst28____git,_X(!0,_X(_cst29____git,_X(!0,_X(_cst30____git,_X(!1,_X(_cst31____git,_X(_a,null))))))))))))))));
_b = _bb10___git_23_git_2d_archive;
_r = _s[_t-2];
return _p(_bb9___git_23_git_2d_archive);
},3,1);

_l(_bb10___git_23_git_2d_archive = () => {
if (_n !== 1) {
return _w(_bb10___git_23_git_2d_archive);
}
_s[_t+1] = _r;
_s[_t+2] = _a;
_t += 2;
return _p(_bb11___git_23_git_2d_archive);
},-1);

_j(_bb9___git_23_git_2d_archive = () => {
_n = 2;
_t -= 3;
return _g["##call-with-input-process"]();
});

_j(_bb11___git_23_git_2d_archive = () => {
_r = _bb12___git_23_git_2d_archive;
_n = 1;
return _g["##process-status"]();
});

_k(_bb12___git_23_git_2d_archive = () => {
if (_y(_a)) {
return _bb13___git_23_git_2d_archive();
} else {
return _bb18___git_23_git_2d_archive();
}
},2,1);

_j(_bb13___git_23_git_2d_archive = () => {
_a = _a === 0;
if (!(_a === !1)) {
return _bb15___git_23_git_2d_archive();
} else {
return _bb17___git_23_git_2d_archive();
}
});

_j(_bb18___git_23_git_2d_archive = () => {
_b = 0;
_r = _bb14___git_23_git_2d_archive;
_n = 2;
return _e["##="]();
});

_j(_bb15___git_23_git_2d_archive = () => {
_a = _s[_t];
_r = _s[_t-1];
return _p(_bb16___git_23_git_2d_archive);
});

_j(_bb17___git_23_git_2d_archive = () => {
_t -= 2;
return _s[_t+1]();
});

_k(_bb14___git_23_git_2d_archive = () => {
if (!(_a === !1)) {
return _bb15___git_23_git_2d_archive();
} else {
return _bb17___git_23_git_2d_archive();
}
},2,1);

_j(_bb16___git_23_git_2d_archive = () => {
_n = 1;
_t -= 2;
return _g["_tar#tar-unpack-port"]();
});



_m(_bb1___git_23_git_2d_clone = () => {
if (_n === 2) {
_s[++_t] = _a;
_a = _b;
_b = _absent_obj;
_c = _absent_obj;
} else {
if (_n === 3) {
_s[++_t] = _a;
_a = _b;
_b = _c;
_c = _absent_obj;
} else {
if (_n !== 4) {
return _w(_bb1___git_23_git_2d_clone);
}
}
}
if (_c === _absent_obj) {
return _bb2___git_23_git_2d_clone();
} else {
return _bb3___git_23_git_2d_clone();
}
},-1,_i("_git#git-clone"),!1,!0);

_j(_bb2___git_23_git_2d_clone = () => {
_d = !1;
if (_b === _absent_obj) {
return _bb5___git_23_git_2d_clone();
} else {
return _bb4___git_23_git_2d_clone();
}
});

_j(_bb3___git_23_git_2d_clone = () => {
_d = _c;
if (_b === _absent_obj) {
return _bb5___git_23_git_2d_clone();
} else {
return _bb4___git_23_git_2d_clone();
}
});

_j(_bb5___git_23_git_2d_clone = () => {
var closure0 = _closure_alloc([_bb20___git_23_git_2d_clone,_a,_d]);
_s[_t+1] = closure0;
_s[_t+2] = _b;
_b = _s[_t+1];
_s[_t+1] = _s[_t+2];
if (_stringp(_s[_t])) {
++_t;
return _bb6___git_23_git_2d_clone();
} else {
++_t;
return _bb18___git_23_git_2d_clone();
}
});

_j(_bb4___git_23_git_2d_clone = () => {
if (_b === !1) {
return _bb5___git_23_git_2d_clone();
} else {
return _bb17___git_23_git_2d_clone();
}
});

_l(_bb20___git_23_git_2d_clone = () => {
if (_n !== 1) {
return _w(_d);
}
_s[_t+1] = _r;
_s[_t+2] = _a;
_s[_t+3] = _d;
_t += 3;
return _p(_bb21___git_23_git_2d_clone);
},2);

_j(_bb6___git_23_git_2d_clone = () => {
if (_stringp(_a)) {
return _bb7___git_23_git_2d_clone();
} else {
return _bb16___git_23_git_2d_clone();
}
});

_j(_bb18___git_23_git_2d_clone = () => {
_s[_t+1] = _s[_t-1];
_s[_t-1] = 1;
_s[_t+2] = _s[_t];
_s[_t] = _bb1___git_23_git_2d_clone;
_b = _s[_t+2];
_t += 2;
return _p(_bb19___git_23_git_2d_clone);
});

_j(_bb17___git_23_git_2d_clone = () => {
_s[_t+1] = _b;
if (_stringp(_s[_t])) {
++_t;
return _bb6___git_23_git_2d_clone();
} else {
++_t;
return _bb18___git_23_git_2d_clone();
}
});

_j(_bb21___git_23_git_2d_clone = () => {
_r = _bb22___git_23_git_2d_clone;
_n = 1;
return _g["##process-status"]();
});

_j(_bb7___git_23_git_2d_clone = () => {
if (typeof _b === "function" && !Object.prototype.hasOwnProperty.call(_b,"h")) {
return _bb8___git_23_git_2d_clone();
} else {
return _bb14___git_23_git_2d_clone();
}
});

_j(_bb16___git_23_git_2d_clone = () => {
_s[_t+1] = _s[_t-1];
_s[_t-1] = 2;
_s[_t+2] = _s[_t];
_s[_t] = _bb1___git_23_git_2d_clone;
_b = _s[_t+2];
_t += 2;
return _p(_bb19___git_23_git_2d_clone);
});

_j(_bb19___git_23_git_2d_clone = () => {
_n = 6;
--_t;
return _g["##fail-check-string"]();
});

_k(_bb22___git_23_git_2d_clone = () => {
if (_y(_a)) {
return _bb23___git_23_git_2d_clone();
} else {
return _bb38___git_23_git_2d_clone();
}
},3,1);

_j(_bb8___git_23_git_2d_clone = () => {
_s[_t] = _r;
_s[_t+1] = _a;
_s[_t+2] = _b;
_s[_t+3] = _d;
_t += 3;
return _p(_bb9___git_23_git_2d_clone);
});

_j(_bb14___git_23_git_2d_clone = () => {
_s[_t+1] = _s[_t-1];
_s[_t-1] = 3;
_s[_t+2] = _s[_t];
_s[_t] = _bb1___git_23_git_2d_clone;
_b = _s[_t+2];
_t += 2;
return _p(_bb15___git_23_git_2d_clone);
});

_j(_bb23___git_23_git_2d_clone = () => {
if (_a === 0) {
return _bb36___git_23_git_2d_clone();
} else {
return _bb25___git_23_git_2d_clone();
}
});

_j(_bb38___git_23_git_2d_clone = () => {
_b = 0;
_r = _bb24___git_23_git_2d_clone;
_n = 2;
return _e["##="]();
});

_j(_bb9___git_23_git_2d_clone = () => {
_r = _bb10___git_23_git_2d_clone;
_n = 1;
return _g["##file-exists?"]();
});

_j(_bb15___git_23_git_2d_clone = () => {
_n = 6;
--_t;
return _g["##fail-check-procedure"]();
});

_j(_bb36___git_23_git_2d_clone = () => {
_a = _s[_t](!0)[1];
_r = _bb37___git_23_git_2d_clone;
_n = 1;
_t -= 2;
return _g["##path-expand"]();
});

_j(_bb25___git_23_git_2d_clone = () => {
if (!(_s[_t](!0)[2] === !1)) {
return _bb26___git_23_git_2d_clone();
} else {
return _bb28___git_23_git_2d_clone();
}
});

_k(_bb24___git_23_git_2d_clone = () => {
if (!(_a === !1)) {
return _bb36___git_23_git_2d_clone();
} else {
return _bb25___git_23_git_2d_clone();
}
},3,1);

_k(_bb10___git_23_git_2d_clone = () => {
if (!(_a === !1)) {
return _bb13___git_23_git_2d_clone();
} else {
return _bb11___git_23_git_2d_clone();
}
},5,2);

_k(_bb37___git_23_git_2d_clone = () => {
_a = new _Structure([_cst7____git,_a]);
--_t;
return _s[_t+1]();
},1,1);

_j(_bb26___git_23_git_2d_clone = () => {
_t -= 2;
return _bb27___git_23_git_2d_clone();
});

_j(_bb28___git_23_git_2d_clone = () => {
var closure1 = _closure_alloc([_bb30___git_23_git_2d_clone,_s[_t-1]]);
_s[_t] = closure1;
_a = _s[_t];
_r = _bb29___git_23_git_2d_clone;
_n = 1;
_t -= 2;
return _g["##repl"]();
});

_j(_bb13___git_23_git_2d_clone = () => {
_a = !1;
_t -= 5;
return _s[_t+2]();
});

_j(_bb11___git_23_git_2d_clone = () => {
_s[_t-4] = _X(_cst42____git,_X(_s[_t-4],_X(_s[_t-2],null)));
_c = _s[_t];
_a = _s[_t-1];
_b = !1;
_r = _s[_t-3];
return _p(_bb12___git_23_git_2d_clone);
});

_j(_bb27___git_23_git_2d_clone = () => {
_a = !1;
--_t;
return _s[_t+1]();
});

_l(_bb30___git_23_git_2d_clone = () => {
if (_n !== 2) {
return _w(_d);
}
_s[_t+1] = _r;
_s[_t+2] = _b;
_s[_t+3] = _d;
_a = _b;
_r = _bb31___git_23_git_2d_clone;
_n = 1;
_t += 3;
return _e["##newline1"]();
},1);

_k(_bb29___git_23_git_2d_clone = () => {
return _bb27___git_23_git_2d_clone();
},1,1);

_j(_bb12___git_23_git_2d_clone = () => {
_n = 4;
_t -= 4;
return _bb1___git_23_git_2d_command_2d_aux();
});

_k(_bb31___git_23_git_2d_clone = () => {
_a = _s[_t](!0)[1];
_b = !1;
return _p(_bb32___git_23_git_2d_clone);
},3,1);

_j(_bb32___git_23_git_2d_clone = () => {
_r = _bb33___git_23_git_2d_clone;
_n = 2;
--_t;
return _g["##read-line"]();
});

_k(_bb33___git_23_git_2d_clone = () => {
_b = _s[_t];
_r = _bb34___git_23_git_2d_clone;
_n = 2;
return _g["##write-string"]();
},2,1);

_k(_bb34___git_23_git_2d_clone = () => {
_a = _s[_t];
_r = _bb35___git_23_git_2d_clone;
_n = 1;
--_t;
return _e["##newline1"]();
},2,1);

_k(_bb35___git_23_git_2d_clone = () => {
_a = !0;
--_t;
return _s[_t+1]();
},1,1);



_m(_bb1___git_23_git_2d_pull = () => {
if (_n === 1) {
_b = _absent_obj;
_c = _absent_obj;
} else {
if (_n === 2) {
_c = _absent_obj;
} else {
if (_n !== 3) {
return _w(_bb1___git_23_git_2d_pull);
}
}
}
if (_b === _absent_obj) {
return _bb2___git_23_git_2d_pull();
} else {
return _bb3___git_23_git_2d_pull();
}
},-1,_i("_git#git-pull"),!1,!0);

_j(_bb2___git_23_git_2d_pull = () => {
_d = _bb12___git_23_git_2d_pull;
if (_c === _absent_obj) {
return _bb4___git_23_git_2d_pull();
} else {
return _bb9___git_23_git_2d_pull();
}
});

_j(_bb3___git_23_git_2d_pull = () => {
_d = _b;
if (_c === _absent_obj) {
return _bb4___git_23_git_2d_pull();
} else {
return _bb9___git_23_git_2d_pull();
}
});

_l(_bb12___git_23_git_2d_pull = () => {
if (_n !== 1) {
return _w(_bb12___git_23_git_2d_pull);
}
_s[_t+1] = _r;
++_t;
return _p(_bb13___git_23_git_2d_pull);
},-1);

_j(_bb4___git_23_git_2d_pull = () => {
_s[_t+1] = _a;
_a = !1;
if (typeof _d === "function" && !Object.prototype.hasOwnProperty.call(_d,"h")) {
++_t;
return _bb5___git_23_git_2d_pull();
} else {
++_t;
return _bb10___git_23_git_2d_pull();
}
});

_j(_bb9___git_23_git_2d_pull = () => {
_s[_t+1] = _a;
_a = _c;
if (typeof _d === "function" && !Object.prototype.hasOwnProperty.call(_d,"h")) {
++_t;
return _bb5___git_23_git_2d_pull();
} else {
++_t;
return _bb10___git_23_git_2d_pull();
}
});

_j(_bb13___git_23_git_2d_pull = () => {
_r = _bb14___git_23_git_2d_pull;
_n = 1;
return _g["##process-status"]();
});

_j(_bb5___git_23_git_2d_pull = () => {
if (_s[_t] instanceof _Structure && _s[_t].a[0].a[1] === _cst17____git) {
return _bb6___git_23_git_2d_pull();
} else {
return _bb8___git_23_git_2d_pull();
}
});

_j(_bb10___git_23_git_2d_pull = () => {
_s[_t+1] = _s[_t];
_s[_t] = 2;
_s[_t+2] = _s[_t+1];
_s[_t+1] = _bb1___git_23_git_2d_pull;
_a = _s[_t+2];
_t += 2;
return _p(_bb11___git_23_git_2d_pull);
});

_k(_bb14___git_23_git_2d_pull = () => {
if (_y(_a)) {
return _bb17___git_23_git_2d_pull();
} else {
return _bb15___git_23_git_2d_pull();
}
},1,1);

_j(_bb6___git_23_git_2d_pull = () => {
_b = _X(_cst46____git,_X(_cst47____git,null));
_s[_t+1] = _s[_t];
_s[_t] = _b;
_c = _a;
_b = _s[_t+1];
_a = _d;
++_t;
return _p(_bb7___git_23_git_2d_pull);
});

_j(_bb8___git_23_git_2d_pull = () => {
_a = !1;
--_t;
return _r;
});

_j(_bb11___git_23_git_2d_pull = () => {
_n = 5;
--_t;
return _g["##fail-check-procedure"]();
});

_j(_bb17___git_23_git_2d_pull = () => {
_a = _a === 0;
--_t;
return _s[_t+1]();
});

_j(_bb15___git_23_git_2d_pull = () => {
_b = 0;
_r = _s[_t];
return _p(_bb16___git_23_git_2d_pull);
});

_j(_bb7___git_23_git_2d_pull = () => {
_n = 4;
--_t;
return _bb1___git_23_git_2d_command_2d_aux();
});

_j(_bb16___git_23_git_2d_pull = () => {
_n = 2;
--_t;
return _e["##="]();
});



_m(_bb1___git_23_git_2d_status = () => {
if (_n === 1) {
_b = _absent_obj;
_c = _absent_obj;
} else {
if (_n === 2) {
_c = _absent_obj;
} else {
if (_n !== 3) {
return _w(_bb1___git_23_git_2d_status);
}
}
}
if (_b === _absent_obj) {
return _bb2___git_23_git_2d_status();
} else {
return _bb3___git_23_git_2d_status();
}
},-1,_i("_git#git-status"),!1,!0);

_j(_bb2___git_23_git_2d_status = () => {
_d = _bb12___git_23_git_2d_status;
if (_c === _absent_obj) {
return _bb4___git_23_git_2d_status();
} else {
return _bb9___git_23_git_2d_status();
}
});

_j(_bb3___git_23_git_2d_status = () => {
_d = _b;
if (_c === _absent_obj) {
return _bb4___git_23_git_2d_status();
} else {
return _bb9___git_23_git_2d_status();
}
});

_l(_bb12___git_23_git_2d_status = () => {
if (_n !== 1) {
return _w(_bb12___git_23_git_2d_status);
}
_s[_t+1] = _r;
++_t;
return _p(_bb13___git_23_git_2d_status);
},-1);

_j(_bb4___git_23_git_2d_status = () => {
_s[_t+1] = _a;
_a = !1;
if (typeof _d === "function" && !Object.prototype.hasOwnProperty.call(_d,"h")) {
++_t;
return _bb5___git_23_git_2d_status();
} else {
++_t;
return _bb10___git_23_git_2d_status();
}
});

_j(_bb9___git_23_git_2d_status = () => {
_s[_t+1] = _a;
_a = _c;
if (typeof _d === "function" && !Object.prototype.hasOwnProperty.call(_d,"h")) {
++_t;
return _bb5___git_23_git_2d_status();
} else {
++_t;
return _bb10___git_23_git_2d_status();
}
});

_j(_bb13___git_23_git_2d_status = () => {
_r = _bb14___git_23_git_2d_status;
_n = 1;
return _g["##process-status"]();
});

_j(_bb5___git_23_git_2d_status = () => {
if (_s[_t] instanceof _Structure && _s[_t].a[0].a[1] === _cst17____git) {
return _bb6___git_23_git_2d_status();
} else {
return _bb8___git_23_git_2d_status();
}
});

_j(_bb10___git_23_git_2d_status = () => {
_s[_t+1] = _s[_t];
_s[_t] = 2;
_s[_t+2] = _s[_t+1];
_s[_t+1] = _bb1___git_23_git_2d_status;
_a = _s[_t+2];
_t += 2;
return _p(_bb11___git_23_git_2d_status);
});

_k(_bb14___git_23_git_2d_status = () => {
if (_y(_a)) {
return _bb17___git_23_git_2d_status();
} else {
return _bb15___git_23_git_2d_status();
}
},1,1);

_j(_bb6___git_23_git_2d_status = () => {
_b = _X(_cst48____git,null);
_s[_t+1] = _s[_t];
_s[_t] = _b;
_c = _a;
_b = _s[_t+1];
_a = _d;
++_t;
return _p(_bb7___git_23_git_2d_status);
});

_j(_bb8___git_23_git_2d_status = () => {
_a = !1;
--_t;
return _r;
});

_j(_bb11___git_23_git_2d_status = () => {
_n = 5;
--_t;
return _g["##fail-check-procedure"]();
});

_j(_bb17___git_23_git_2d_status = () => {
_a = _a === 0;
--_t;
return _s[_t+1]();
});

_j(_bb15___git_23_git_2d_status = () => {
_b = 0;
_r = _s[_t];
return _p(_bb16___git_23_git_2d_status);
});

_j(_bb7___git_23_git_2d_status = () => {
_n = 4;
--_t;
return _bb1___git_23_git_2d_command_2d_aux();
});

_j(_bb16___git_23_git_2d_status = () => {
_n = 2;
--_t;
return _e["##="]();
});



_m(_bb1___git_23_git_2d_tag = () => {
if (_n !== 1) {
return _w(_bb1___git_23_git_2d_tag);
}
if (_a instanceof _Structure && _a.a[0].a[1] === _cst17____git) {
return _bb2___git_23_git_2d_tag();
} else {
return _bb22___git_23_git_2d_tag();
}
},-1,_i("_git#git-tag"),!1,!0);

_j(_bb2___git_23_git_2d_tag = () => {
_b = _X(_cst49____git,null);
if (_a instanceof _Structure && _a.a[0].a[1] === _cst17____git) {
return _bb3___git_23_git_2d_tag();
} else {
return _bb4___git_23_git_2d_tag();
}
});

_j(_bb22___git_23_git_2d_tag = () => {
_a = !1;
return _r;
});

_j(_bb3___git_23_git_2d_tag = () => {
_a = _a.a[1];
return _bb5___git_23_git_2d_tag();
});

_j(_bb4___git_23_git_2d_tag = () => {
_a = !1;
return _bb5___git_23_git_2d_tag();
});

_j(_bb5___git_23_git_2d_tag = () => {
_s[_t+1] = _r;
_s[_t+2] = _a;
_s[_t+3] = _b;
_t += 3;
return _p(_bb6___git_23_git_2d_tag);
});

_j(_bb6___git_23_git_2d_tag = () => {
_r = _bb7___git_23_git_2d_tag;
_n = 0;
return _g["##tty-mode-reset"]();
});

_k(_bb7___git_23_git_2d_tag = () => {
_r = _bb8___git_23_git_2d_tag;
_n = 0;
return _g["##os-environ"]();
},3,1);

_k(_bb8___git_23_git_2d_tag = () => {
_a = _X(_cst32____git,_a);
_a = _X(_cst23____git,_X(_cst24____git,_X(_cst25____git,_X(_s[_t],_X(_cst26____git,_X(_s[_t-1],_X(_cst27____git,_X(!1,_X(_cst28____git,_X(!0,_X(_cst29____git,_X(!0,_X(_cst30____git,_X(!1,_X(_cst31____git,_X(_a,null))))))))))))))));
_b = _bb10___git_23_git_2d_tag;
_r = _s[_t-2];
return _p(_bb9___git_23_git_2d_tag);
},3,1);

_l(_bb10___git_23_git_2d_tag = () => {
if (_n !== 1) {
return _w(_bb10___git_23_git_2d_tag);
}
_b = null;
return _p(_bb13___git_23_git_2d_tag);
},-1);

_j(_bb9___git_23_git_2d_tag = () => {
_n = 2;
_t -= 3;
return _g["##call-with-input-process"]();
});

_j(_bb13___git_23_git_2d_tag = () => {
_s[_t+1] = _r;
_s[_t+2] = _a;
_s[_t+3] = _b;
_t += 3;
return _p(_bb14___git_23_git_2d_tag);
});

_j(_bb14___git_23_git_2d_tag = () => {
_r = _bb15___git_23_git_2d_tag;
_n = 1;
return _g["##read-line"]();
});

_k(_bb15___git_23_git_2d_tag = () => {
if (_a === _eof_obj) {
return _bb21___git_23_git_2d_tag();
} else {
return _bb16___git_23_git_2d_tag();
}
},3,1);

_j(_bb21___git_23_git_2d_tag = () => {
_a = _s[_t];
_t -= 3;
return _s[_t+1]();
});

_j(_bb16___git_23_git_2d_tag = () => {
_a = _X(_a,_s[_t]);
_s[_t] = _a;
_a = _s[_t-1];
_r = _bb17___git_23_git_2d_tag;
_n = 1;
return _g["##read-line"]();
});

_k(_bb17___git_23_git_2d_tag = () => {
if (_a === _eof_obj) {
return _bb21___git_23_git_2d_tag();
} else {
return _bb18___git_23_git_2d_tag();
}
},3,1);

_j(_bb18___git_23_git_2d_tag = () => {
_a = _X(_a,_s[_t]);
_s[_t] = _a;
_a = _s[_t-1];
_r = _bb19___git_23_git_2d_tag;
_n = 1;
return _g["##read-line"]();
});

_k(_bb19___git_23_git_2d_tag = () => {
if (_a === _eof_obj) {
return _bb21___git_23_git_2d_tag();
} else {
return _bb20___git_23_git_2d_tag();
}
},3,1);

_j(_bb20___git_23_git_2d_tag = () => {
_a = _X(_a,_s[_t]);
_s[_t] = _a;
_a = _s[_t-1];
_r = _bb11___git_23_git_2d_tag;
_n = 1;
return _g["##read-line"]();
});

_k(_bb11___git_23_git_2d_tag = () => {
if (_a === _eof_obj) {
return _bb21___git_23_git_2d_tag();
} else {
return _bb12___git_23_git_2d_tag();
}
},3,1);

_j(_bb12___git_23_git_2d_tag = () => {
_b = _X(_a,_s[_t]);
_a = _s[_t-1];
_r = _s[_t-2];
_t -= 3;
return _p(_bb13___git_23_git_2d_tag);
});



_module_register([[_i("_git")],[_i("_git"),_i("_tar")],null,1,_bb1___git_23_,!1]);

