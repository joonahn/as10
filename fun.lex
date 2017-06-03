type pos = ErrorMsg.pos
type svalue = Tokens.svalue
type ('svalue,'pos) token = ('svalue,'pos) Tokens.token
type lexresult  = (svalue,ErrorMsg.pos) token

val newLine = ErrorMsg.newLine
val commentLevel : int ref = ref 0

fun inc_comment () : int = 
	let val y:int = (!commentLevel) + 1 in
		commentLevel:= (!commentLevel) + 1;
		y
	end

fun dec_comment () : int = 
	let val y:int = (!commentLevel) - 1 in
		commentLevel:= (!commentLevel) - 1;
		y
	end

fun make_pos (yypos,yytext) : ErrorMsg.pos2
    = (yypos, yypos + String.size(yytext) - 1)

fun make_int_args (yypos, yytext) : int * ErrorMsg.pos * ErrorMsg.pos = 
	let
		val (pos1, pos2) = make_pos(yypos, yytext)
		val value        = valOf(Int.fromString(yytext))
	in
		(value, pos1, pos2)
	end

fun make_id_args (yypos, yytext) : string * ErrorMsg.pos * ErrorMsg.pos = 
	let 
		val (pos1, pos2) = make_pos(yypos, yytext)
		val str = yytext
	in
		(str, pos1, pos2)
	end

fun make_proj_args (yypos, yytext) : int * ErrorMsg.pos * ErrorMsg.pos = 
	let 
		val (pos1, pos2) = make_pos(yypos, yytext)
		val value = valOf(Int.fromString(Substring.string(Substring.extract(yytext, 1, NONE))))
	in
		(value, pos1, pos2)
	end

(* Handling EOF.  Note that this function reports the wrong file-position for
   end-of-file.  Because of a design infelicity of ML-Lex, it's possible but
   not easy to get access to the correct file position.  There is a way to 
   do it using the %arg feature of ML-Lex, but you don't need to bother 
   with it for this exercise. 
*)
fun eof () = 
     Tokens.EOF(0,0)

%%

%s COMMENT;
%header (functor FunLexFun(structure Tokens: Fun_TOKENS));

alpha = [A-Za-z];

%%
<INITIAL>fun   => (Tokens.FUN(make_pos(yypos,yytext)));
<INITIAL>\n    => (newLine yypos; continue ());
<INITIAL>(" "|\t|\r) => (continue());

<INITIAL>if    => (Tokens.IF(make_pos(yypos,yytext)));
<INITIAL>then  => (Tokens.THEN(make_pos(yypos,yytext)));
<INITIAL>else  => (Tokens.ELSE(make_pos(yypos,yytext)));
<INITIAL>in  => (Tokens.IN(make_pos(yypos,yytext)));
<INITIAL>let  => (Tokens.LET(make_pos(yypos,yytext)));
<INITIAL>ref  => (Tokens.REF(make_pos(yypos,yytext)));
<INITIAL>do  => (Tokens.DO(make_pos(yypos,yytext)));
<INITIAL>while  => (Tokens.WHILE(make_pos(yypos,yytext)));
<INITIAL>not  => (Tokens.NOT(make_pos(yypos,yytext)));
<INITIAL>"||"  => (Tokens.OR(make_pos(yypos,yytext)));
<INITIAL>"&"  => (Tokens.AND(make_pos(yypos,yytext)));
<INITIAL>":="  => (Tokens.ASSIGN(make_pos(yypos,yytext)));
<INITIAL>"!"  => (Tokens.BANG(make_pos(yypos,yytext)));
<INITIAL>"->"  => (Tokens.ARROW(make_pos(yypos,yytext)));
<INITIAL>">"  => (Tokens.GT(make_pos(yypos,yytext)));
<INITIAL>"<"  => (Tokens.LT(make_pos(yypos,yytext)));
<INITIAL>"="  => (Tokens.EQ(make_pos(yypos,yytext)));
<INITIAL>"*"  => (Tokens.TIMES(make_pos(yypos,yytext)));
<INITIAL>"-"  => (Tokens.MINUS(make_pos(yypos,yytext)));
<INITIAL>"+"  => (Tokens.PLUS(make_pos(yypos,yytext)));
<INITIAL>"("  => (Tokens.LPAREN(make_pos(yypos,yytext)));
<INITIAL>")"  => (Tokens.RPAREN(make_pos(yypos,yytext)));
<INITIAL>":"  => (Tokens.COLON(make_pos(yypos,yytext)));
<INITIAL>";"  => (Tokens.SEMICOLON(make_pos(yypos,yytext)));
<INITIAL>","  => (Tokens.COMMA(make_pos(yypos,yytext)));
<INITIAL>[0-9]+ => (Tokens.INT(make_int_args(yypos, yytext)));
<INITIAL>[A-Za-z][A-Za-z0-9_]* => (Tokens.ID(make_id_args(yypos, yytext)));
<INITIAL>[0-9][A-Za-z]+ => (ErrorMsg.error(make_pos(yypos, yytext), "ID syntax error"); continue());
<INITIAL>#[1-9][0-9]* => (Tokens.PROJ(make_proj_args(yypos, yytext)));
<INITIAL>#0 => (Tokens.PROJ(make_proj_args(yypos, yytext)));
<INITIAL>#0[0-9]+ => (ErrorMsg.error(make_pos(yypos, yytext), "proj number error"); continue());

<INITIAL>"/*"  => (YYBEGIN COMMENT;inc_comment(); continue());
<COMMENT>"/*"  => (inc_comment(); continue());
<COMMENT>"*/"  => (if dec_comment()=0 then YYBEGIN INITIAL else (); continue());
<COMMENT>\n    => (newLine yypos; continue ());
<COMMENT>.  => (continue());