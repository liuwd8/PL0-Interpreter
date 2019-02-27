# 				   实验报告

## 实验任务一
### 修改未在ascii码中定义的符号
1. 本实验在来时原来的代码上，重新定义 `≥` 为 `>=` 、`≤` 为 `<=` 、 `≠` 为 `<>`, 并修改了提供的 `pl0` 程序中，这三个符号的表达。
   针对上述三个符号，修改了一下代码附近的语句:
    ```pascal
    {主程序块中, 修改 ≥ 为 >=, ≤ 为 <=, 不等于为 <>}
    {在这里删除了对应的符号转换，更改到 `getsym` 中}
    ssym['+'] := plus;      ssym['-'] := minus;
    ssym['*'] := times;     ssym['/'] := slash;
    ssym['('] := lparen;     ssym[')'] := rparen;
    ssym['='] := eql;       ssym[','] := comma;
    ssym['.'] := period;     ssym[';'] := semicolon;
    ```
    ```pascal
    {`getsym` 中为了处理上述三个符号增加的语句}
    if ch = '<' then
    begin
        getch;
        if ch = '=' then
        begin
        sym := leq; getch
        end else if ch = '>' then
        begin
        sym := neq; getch
        end else sym := lss
    end else if ch = '>' then
    begin
        getch;
        if ch = '=' then
        begin
        sym := geq; getch
        end
        else sym := gtr
    end
    ```
2. 修改了其他非ascii码中的符号，更改 `error` 中的 `↑` 为 `@`。
3. 修改解析数字时的`ord(0)`为字符`ord('0')`
   ```pascal
    begin {数字} 
        k := 0;  num := 0;  sym := number;
        repeat
            num := 10*num + (ord(ch)-ord('0'));
            k := k + 1;  getch;
        until not (ch in ['0'..'9']);
        if k > nmax then  error(30)
    end
   ```
### 将源程序转换为大写
因为保留字字典中，关键字全为大写，所以需要把源程序转换为大写。修改 `getch`
```pascal
  while not eoln(input) do
  begin
    ll := ll + 1; read(ch);
    line[ll] := upcase(ch)
  end;
```
## 实验任务二
增加 `readsym`, `writesym`, 修改 `norw` 为 `13`, 修改保留字字典：
```pascal
  {按字典序重新排列保留字}
  reservedword[1] := 'BEGIN     '; reservedword[2] := 'CALL      ';
  reservedword[3] := 'CONST     '; reservedword[4] := 'DO        ';
  reservedword[5] := 'END       '; reservedword[6] := 'IF        ';
  reservedword[7] := 'ODD       '; reservedword[8] := 'PROCEDURE ';
  reservedword[9] := 'READ      '; reservedword[10] := 'THEN      ';
  reservedword[11] := 'VAR       '; reservedword[12] := 'WHILE     ';
  reservedword[13] := 'WRITE     ';
  {对应保留字更改 wsym}
  wsym[1] := beginsym;   wsym[2] := callsym;
  wsym[3] := constsym;   wsym[4] := dosym;
  wsym[5] := endsym;    wsym[6] := ifsym;
  wsym[7] := oddsym;    wsym[8] := procsym;
  wsym[9] := readsym;    wsym[10] := thensym;
  wsym[11] := varsym;    wsym[12] := whilesym;
  wsym[13] := writesym;
```
增加功能码 `rd`, `wrt`,修改指令:
```pascal
  mnemonic[lit] := 'LIT';     mnemonic[opr] := 'OPR';
  mnemonic[lod] := 'LOD';    mnemonic[sto] := 'STO';
  mnemonic[cal] := 'CAL';    mnemonic[int] := 'INT';
  mnemonic[jmp] := 'JMP';    mnemonic[jpc] := 'JPC';
  mnemonic[rd] := 'RD';    mnemonic[wrt] := 'WRT';
```
在`statbegsys`集合中，增加`readsym`, `writesym`:
```pascal
  statbegsys := [beginsym, callsym, ifsym, whilesym, readsym, writesym];
```
在`statement`中增加语句解析 `readsym`, `writesym`:
```pascal
    if sym = readsym then
    begin
      getsym;
      if sym = lparen then
      begin
        repeat
          getsym;
          if sym = ident then
          begin
            i := position(id);
            if i = 0 then error(11)
            else if table[i].kind <> variable then
            begin error(12); i := 0 end;
            if i <> 0 then
              with table[i] do
                gen(rd, lev-level, adr)
          end;
          getsym
        until sym <> comma;
        if sym = rparen then getsym else error(33)
      end else error(34)
    end else if sym = writesym then
    begin
      getsym;
      if sym = lparen then
      begin
        repeat
          getsym;
          expression([rparen, comma] + fsys);
          gen(wrt, 0, 0);
        until sym <> comma;
        if sym = rparen then getsym else error(33)
      end else error(34)
    end;
```
在`interpret`中case语句中增加`rd`, `wrt`
```pascal
    rd : read(s[base(l) + a]);
    wrt : begin
        writeln(s[t]); t := t - 1
    end
```
## 修改后的代码
```pascal
program  PL0 ( input, output);
{带有代码生成的PL0编译程序}
label 99;
const
  norw = 13; {保留字的个数, 增加 read, write为保留字, 保留字拓展到13个}
  txmax = 100; {标识符表长度}
  nmax = 14; {数字的最大位数}
  al = 10; {标识符的长度}
  amax = 2047; {最大地址}
  levmax = 3; {程序体嵌套的最大深度}
  cxmax = 200; {代码数组的大小}
type
  symbol = (nul, ident, number, plus, minus, times, slash, oddsym,
  eql, neq, lss, leq, gtr, geq, lparen, rparen, comma, semicolon,
  period, becomes, beginsym, endsym, ifsym, thensym,
  whilesym, dosym, callsym, constsym, varsym, procsym, readsym, writesym);
  {增加 readsym 和 writesym}
  alfa = packed array [1..al] of char;
  myobject = (constant, variable, PL0procedure);
  symset = set of symbol;
  fct = (lit, opr, lod, sto, cal, int, jmp, jpc, rd, wrt); {functions, 增加功能码 rd, wrt}
  instruction = packed record
    f : fct;  {功能码}
    l : 0..levmax; {相对层数}
    a : 0..amax; {相对地址}
  end;
  {LIT 0,a : 取常数a
  OPR 0,a : 执行运算a
  LOD l,a : 取层差为l的层﹑相对地址为a的变量
  STO l,a : 存到层差为l的层﹑相对地址为a的变量
  CAL l,a : 调用层差为l的过程
  INT 0,a : t寄存器增加a
  JMP 0,a : 转移到指令地址a处
  JPC 0,a : 条件转移到指令地址a处 }
var
  ch : char; {最近读到的字符}
  sym : symbol; {最近读到的符号}
  id : alfa; {最近读到的标识符}
  num : integer; {最近读到的数}
  cc : integer; {当前行的字符计数}
  ll : integer; {当前行的长度}
  kk, err : integer;
  cx : integer; {代码数组的当前下标}
  line : array [1..81] of char;
  a : alfa;
  code : array [0..cxmax] of instruction;
  reservedword : array [1..norw] of alfa;
  wsym : array [1..norw] of symbol;
  ssym : array [ char ] of symbol;
  mnemonic : array [fct] of packed array [1..5] of char;
  declbegsys, statbegsys, facbegsys : symset;
  table : array [0..txmax] of record
    name : alfa;
    case kind : myobject of
      constant : (val : integer);
      variable, PL0procedure : (level, adr : integer)
  end;
  stdin : text;
procedure error (n : integer);
begin
  writeln('****', ' ' : cc-1, '@', n : 2);  err := err + 1
end {error};
procedure getsym;
  var  i, j, k : integer;
  procedure  getch ;
  begin
    if cc = ll then
    begin
      if eof(input) then
      begin
        write('PROGRAM INCOMPLETE'); goto 99
      end;
      ll := 0; cc := 0; write(cx : 5, ' ');
      while not eoln(input) do
      begin
        ll := ll + 1; read(ch); write(ch);
        line[ll] := upcase(ch)
      end;
      writeln; ll := ll + 1; read(line[ll])
    end;
    cc := cc + 1; ch := line[cc]
  end {getch};
begin {getsym}
  while ch = ' ' do getch;
  if ch in ['A'..'Z'] then
  begin {标识符或保留字}
    k := 0;
    {读取标识符}
    repeat
      if k < al then
      begin
        k:= k + 1; a[k] := ch
      end;
      getch
    until not (ch in ['A'..'Z', '0'..'9']);
    if k >= kk  then kk := k else
      repeat
        a[kk] := ' '; kk := kk-1
      until kk = k;
    id := a;  i := 1;  j := norw;
    repeat
      k := (i+j) div 2;
      if id <= reservedword[k] then j := k-1;
      if id >= reservedword[k] then i := k + 1
    until i > j;
    if i-1 > j then sym := wsym[k] else sym := ident
  end else if ch in ['0'..'9'] then
  begin {数字} 
    k := 0;  num := 0;  sym := number;
    repeat
      num := 10*num + (ord(ch)-ord('0'));
      k := k + 1;  getch;
    until not (ch in ['0'..'9']);
    if k > nmax then  error(30)
  end else if ch = ':' then
  begin
    getch;
    if ch = '=' then
    begin
      sym := becomes; getch
    end
    else sym := nul
  end else if ch = '<' then
  begin
    getch;
    if ch = '=' then
    begin
      sym := leq; getch
    end else if ch = '>' then
    begin
      sym := neq; getch
    end else sym := lss
  end else if ch = '>' then
  begin
    getch;
    if ch = '=' then
    begin
      sym := geq; getch
    end
    else sym := gtr
  end else
  begin
    sym := ssym[ch];  getch
  end;
end {getsym};
procedure  gen(x : fct; y, z : integer);
begin
  if cx > cxmax then 
  begin
    write('PROGRAM TOO LONG'); goto 99
  end;
  with code[cx] do
  begin
    f := x;  l := y;  a := z
  end;
  cx := cx + 1
end {gen};
procedure  test(s1, s2 : symset; n : integer);
begin
  if not (sym in s1) then
  begin
    error(n);  s1 := s1 + s2;
    while not (sym in s1) do getsym
  end
end {test};
procedure  block(lev, tx : integer; fsys : symset);
  var
    dx : integer; {本过程数据空间分配下标}
    tx0 : integer; {本过程标识表起始下标}
    cx0 : integer; {本过程代码起始下标}
  procedure  enter(k : myobject);
  begin {把myobject填入符号表中}
    tx := tx +1;
    with table[tx] do
    begin
      name := id;  kind := k;
      case k of
        constant : begin
          if num > amax then
          begin error(30); num := 0 end;
          val := num
        end;
        variable : begin
          level := lev;  adr := dx;  dx := dx +1;
        end;
        PL0procedure : level := lev
      end
    end
  end {enter};
  function position(id : alfa) : integer;
    var  i : integer;
  begin {在标识符表中查标识符id}
    table[0].name := id;  i := tx;
    while table[i].name <> id do i := i-1;
    position := i
  end {position};
  procedure constdeclaration;
  begin
    if sym = ident then
    begin
      getsym;
      if sym in [eql, becomes] then
      begin
        if sym = becomes then error(1);
        getsym;
        if sym = number then 
        begin
          enter(constant); getsym
        end
        else error(2)
      end else error(3)
    end else error(4)
  end {constdeclaration};
  procedure  vardeclaration;
  begin
    if sym = ident then
    begin
      enter(variable);  getsym
    end else error(4)
  end {vardeclaration};
  procedure  listcode;
    var  i : integer;
  begin  {列出本程序体生成的代码}
    for i := cx0 to cx-1 do
      with code[i] do
        writeln(i, mnemonic[f] : 5, l : 3, a : 5)
  end {listcode};
  procedure  statement(fsys : symset);
    var  i, cx1, cx2 : integer;
    procedure  expression(fsys : symset);
      var  addop : symbol;
      procedure  term(fsys : symset);
        var  mulop : symbol;
        procedure  factor(fsys : symset);
          var i : integer;
        begin
          test(facbegsys, fsys, 24);
          while sym in facbegsys do
          begin
            if sym = ident then
            begin
              i := position(id);
              if i = 0 then error(11) else
                with table[i] do
                  case kind of
                    constant : gen(lit, 0, val);
                    variable : gen(lod, lev-level, adr);
                    PL0procedure : error(21)
                  end;
              getsym
            end else if sym = number then
            begin
              if num > amax then
              begin
                error(30); num := 0
              end;
              gen(lit, 0, num); getsym
            end else if sym = lparen then
            begin
              getsym;
              expression([rparen]+fsys);
              if sym = rparen then getsym
              else error(22)
            end;
            test(fsys, [lparen], 23)
          end
        end {factor};
      begin {term}
        factor(fsys+[times, slash]);
        while sym in [times, slash] do
        begin
          mulop := sym;  getsym;
          factor(fsys+[times, slash]);
          if mulop = times then gen(opr, 0, 4)
          else gen(opr, 0, 5)
        end
      end {term};
    begin {expression}
      if sym in [plus, minus] then
      begin 
        addop := sym;  getsym;
        term(fsys+[plus, minus]);
        if addop = minus then gen(opr, 0, 1)
      end else term(fsys+[plus, minus]);
      while sym in [plus, minus] do
      begin
        addop := sym;  getsym;
        term(fsys+[plus, minus]);
        if addop = plus then gen(opr, 0, 2)
        else gen(opr, 0, 3)
      end
    end {expression};
    procedure  condition(fsys : symset);
      var  relop : symbol;
    begin
      if sym = oddsym then 
      begin
        getsym;  expression(fsys);  gen(opr, 0, 6)
      end else
      begin
        expression([eql, neq, lss, gtr, leq, geq] + fsys);
        if not (sym in [eql, neq, lss, leq, gtr, geq]) then error(20)
        else begin
          relop := sym;  getsym;  expression(fsys);
          case relop of
            eql : gen(opr, 0, 8);
            neq : gen(opr, 0, 9);
            lss : gen(opr, 0, 10);
            geq : gen(opr, 0, 11);
            gtr : gen(opr, 0, 12);
            leq : gen(opr, 0, 13);
          end
        end
      end
    end {condition};
  begin {statement}
    if sym = ident then
    begin
      i := position(id);
      if i = 0 then error(11)
      else if table[i].kind <> variable then
      begin {对非变量赋值} error(12); i := 0 end;
      getsym;
      if sym = becomes then getsym else error(13);
      expression(fsys);
      if i <> 0 then
        with table[i] do
          gen(sto, lev-level, adr)
    end else if sym = callsym then
    begin
      getsym;
      if sym <> ident then error(14)
      else begin 
        i := position(id);
        if i = 0 then error(11) else
          with table[i] do
            if kind = PL0procedure then 
              gen(cal, lev-level, adr)
            else error(15);
        getsym
      end
    end else if sym = ifsym then
    begin
      getsym;  condition([thensym, dosym]+fsys);
      if sym = thensym then getsym else error(16);
      cx1 := cx;  gen(jpc, 0, 0);
      statement(fsys);  code[cx1].a := cx
    end else if sym = beginsym then
    begin
      getsym;  statement([semicolon, endsym]+fsys);
      while sym in [semicolon]+statbegsys do
      begin
        if sym = semicolon then getsym else error(10);
        statement([semicolon, endsym]+fsys)
      end;
      if sym = endsym then getsym else error(17)
    end else if sym = whilesym then
    begin
      cx1 := cx;  getsym;  condition([dosym]+fsys);
      cx2 := cx;  gen(jpc, 0, 0);
      if sym = dosym then getsym else error(18);
      statement(fsys);  gen(jmp, 0, cx1);  code[cx2].a := cx
    end else if sym = readsym then
    begin
      getsym;
      if sym = lparen then
      begin
        repeat
          getsym;
          if sym = ident then
          begin
            i := position(id);
            if i = 0 then error(11)
            else if table[i].kind <> variable then
            begin error(12); i := 0 end;
            if i <> 0 then
              with table[i] do
                gen(rd, lev-level, adr)
          end;
          getsym
        until sym <> comma;
        if sym = rparen then getsym else error(33)
      end else error(34)
    end else if sym = writesym then
    begin
      getsym;
      if sym = lparen then
      begin
        repeat
          getsym;
          expression([rparen, comma] + fsys);
          gen(wrt, 0, 0);
        until sym <> comma;
        if sym = rparen then getsym else error(33)
      end else error(34)
    end;
    test(fsys, [], 19)
  end {statement};
begin {block}
  dx := 3;  tx0 := tx;  table[tx].adr := cx;
  gen(jmp, 0, 0);
  if lev > levmax then error(32);
  repeat
    if sym = constsym then 
    begin
      getsym;
      repeat 
        constdeclaration;
        while sym = comma do
        begin getsym; constdeclaration end;
        if sym = semicolon then getsym else error(5)
      until sym <> ident;
    end;
    if sym = varsym then
    begin
      getsym;
      repeat 
        vardeclaration;
        while sym = comma do
        begin  getsym;  vardeclaration  end;
        if sym = semicolon then getsym else error(5)
      until sym <> ident
    end;
    while sym = procsym do
    begin
      getsym;
      if sym = ident then
      begin  enter(PL0procedure);  getsym  end
      else error(4);
      if sym = semicolon then getsym else error(5);
      block(lev+1, tx, [semicolon]+fsys);
      if sym = semicolon then
      begin
        getsym;
        test(statbegsys+[ident, procsym], fsys, 6)
      end
      else error(5)
    end;
    test(statbegsys+[ident], declbegsys, 7)
  until not (sym in declbegsys);
  code[table[tx0].adr].a := cx; {回填 jmp, 指向代码开始地址}
  with table[tx0] do
  begin
    adr := cx; {更改符号表 produce 标识符条目的地址为代码开始地址}
  end;
  cx0 := cx; gen(int, 0, dx);
  statement([semicolon, endsym]+fsys);
  gen(opr, 0, 0); {生成返回指令}
  test(fsys, [], 8);
  listcode;
end  {block};
procedure  interpret;
  const  stacksize = 500;
  var  p, b, t : integer; {程序地址寄存器, 基地址寄存器,栈顶地址寄存器}
      i : instruction; {指令寄存器}
      s : array [1..stacksize] of integer; {数据存储栈}
  function  base(l : integer) : integer;
    var  b1 : integer;
  begin
    b1 := b; {顺静态链求层差为l的层的基地址}
    while l > 0 do
    begin  b1 := s[b1];  l := l-1 end;
    base := b1
  end {base};
begin
  writeln('START PL/0');
  t := 0;  b := 1;  p := 0;
  s[1] := 0;  s[2] := 0;  s[3] := 0; {预留单元,SL,DL,RA}
  repeat
    i := code[p];  p := p+1;
    with i do
      case f of
        lit : begin
          t := t+1;  s[t] := a
        end;
        opr : case a of {运算}
          0 : begin {返回}
            t := b-1;  p := s[t+3];  b := s[t+2];
          end;
          1 : s[t] := -s[t];
          2 : begin
            t := t-1;  s[t] := s[t] + s[t+1]
          end;
          3 : begin
            t := t-1;  s[t] := s[t]-s[t+1]
          end;
          4 : begin
            t := t-1;  s[t] := s[t] * s[t+1]
          end;
          5 : begin
            t := t-1;  s[t] := s[t] div s[t+1]
          end;
          6 : s[t] := ord(odd(s[t]));
          8 : begin  t := t-1;
            s[t] := ord(s[t] = s[t+1])
          end;
          9: begin  t := t-1;
            s[t] := ord(s[t] <> s[t+1])
          end;
          10 : begin  t := t-1;
            s[t] := ord(s[t] < s[t+1])
          end;
          11: begin  t := t-1;
            s[t] := ord(s[t] >= s[t+1])
          end;
          12 : begin  t := t-1;
            s[t] := ord(s[t] > s[t+1])
          end;
          13 : begin  t := t-1;
            s[t] := ord(s[t] <= s[t+1])
          end;
        end;
        lod : begin
          t := t + 1;  s[t] := s[base(l) + a]
        end;
        sto : begin
          s[base(l) + a] := s[t];  writeln(s[t]);
          t := t-1
        end;
        cal : begin {generate new block mark}
          s[t+1] := base( l );  s[t+2] := b;
          s[t+3] := p; {保存上一层的SL,DL,RA的地址}
          b := t+1;  p := a {更改程序基地址}
        end;
        int : t := t + a;
        jmp : p := a;
        jpc : begin
          if s[t] = 0 then p := a;
          t := t-1
        end;
        rd : read(s[base(l) + a]);
        wrt : begin
          writeln(s[t]); t := t - 1
        end
      end {with, case}
  until p = 0;
  write('END PL/0');
end {interpret};
begin  {主程序}
  if paramcount < 1 then begin writeln('缺少参数'); goto 99 end;
  stdin := input;
  assign(input, paramstr(1));
  reset(input);
  for ch := chr(0) to chr(255) do  ssym[ch] := nul;
  {按字典序重新排列保留字}
  reservedword[1] := 'BEGIN     '; reservedword[2] := 'CALL      ';
  reservedword[3] := 'CONST     '; reservedword[4] := 'DO        ';
  reservedword[5] := 'END       '; reservedword[6] := 'IF        ';
  reservedword[7] := 'ODD       '; reservedword[8] := 'PROCEDURE ';
  reservedword[9] := 'READ      '; reservedword[10] := 'THEN      ';
  reservedword[11] := 'VAR       '; reservedword[12] := 'WHILE     ';
  reservedword[13] := 'WRITE     ';
  {对应保留字更改 wsym}
  wsym[1] := beginsym;   wsym[2] := callsym;
  wsym[3] := constsym;   wsym[4] := dosym;
  wsym[5] := endsym;    wsym[6] := ifsym;
  wsym[7] := oddsym;    wsym[8] := procsym;
  wsym[9] := readsym;    wsym[10] := thensym;
  wsym[11] := varsym;    wsym[12] := whilesym;
  wsym[13] := writesym;
  {修改 ≥ 为 >=, ≤ 为 <=, 不等于为 <>}
  ssym['+'] := plus;      ssym['-'] := minus;
  ssym['*'] := times;     ssym['/'] := slash;
  ssym['('] := lparen;     ssym[')'] := rparen;
  ssym['='] := eql;       ssym[','] := comma;
  ssym['.'] := period;     ssym[';'] := semicolon;
  mnemonic[lit] := 'LIT';     mnemonic[opr] := 'OPR';
  mnemonic[lod] := 'LOD';    mnemonic[sto] := 'STO';
  mnemonic[cal] := 'CAL';    mnemonic[int] := 'INT';
  mnemonic[jmp] := 'JMP';    mnemonic[jpc] := 'JPC';
  mnemonic[rd] := 'RD';    mnemonic[wrt] := 'WRT';
  declbegsys := [constsym, varsym, procsym];
  statbegsys := [beginsym, callsym, ifsym, whilesym, readsym, writesym];
  facbegsys := [ident, number, lparen];
  page(output); err := 0;
  cc := 0;  cx := 0;  ll := 0;  ch := ' ';  kk := al;  getsym;
  block(0, 0, [period]+declbegsys+statbegsys);
  close(input);
  input := stdin;
  if  sym <> period then error(9);
  if err = 0 then interpret
  else write('ERRORS IN PL/0 PROGRAM');
99:
  writeln
end.
```
## 任务一输出
```pascal
    0 const  m = 7, n = 85;
    1 var  x, y, z, q, r;
    1 procedure  multiply;
    1   var  a, b;
    2   begin  a := x;  b := y;  z := 0;
    9     while b > 0 do
   13     begin
   13       if odd b then z := z + a;
   20       a := 2*a ;  b := b/2 ;
   28     end
   28   end;
          2INT    0    5
          3LOD    1    3
          4STO    0    3
          5LOD    1    4
          6STO    0    4
          7LIT    0    0
          8STO    1    5
          9LOD    0    4
         10LIT    0    0
         11OPR    0   12
         12JPC    0   29
         13LOD    0    4
         14OPR    0    6
         15JPC    0   20
         16LOD    1    5
         17LOD    0    3
         18OPR    0    2
         19STO    1    5
         20LIT    0    2
         21LOD    0    3
         22OPR    0    4
         23STO    0    3
         24LOD    0    4
         25LIT    0    2
         26OPR    0    5
         27STO    0    4
         28JMP    0    9
         29OPR    0    0
   30 procedure  divide;
   30   var  w;
   31   begin  r := x;  q := 0;  w := y;
   38     while w <= r do w := 2*w ;
   47     while w > y do
   51     begin  q := 2*q;  w := w/2;
   59       if w <= r then
   62       begin  r := r-w;  q := q+1 end
   71     end
   71   end;
         31INT    0    4
         32LOD    1    3
         33STO    1    7
         34LIT    0    0
         35STO    1    6
         36LOD    1    4
         37STO    0    3
         38LOD    0    3
         39LOD    1    7
         40OPR    0   13
         41JPC    0   47
         42LIT    0    2
         43LOD    0    3
         44OPR    0    4
         45STO    0    3
         46JMP    0   38
         47LOD    0    3
         48LOD    1    4
         49OPR    0   12
         50JPC    0   72
         51LIT    0    2
         52LOD    1    6
         53OPR    0    4
         54STO    1    6
         55LOD    0    3
         56LIT    0    2
         57OPR    0    5
         58STO    0    3
         59LOD    0    3
         60LOD    1    7
         61OPR    0   13
         62JPC    0   71
         63LOD    1    7
         64LOD    0    3
         65OPR    0    3
         66STO    1    7
         67LOD    1    6
         68LIT    0    1
         69OPR    0    2
         70STO    1    6
         71JMP    0   47
         72OPR    0    0
   73 procedure  gcd;
   73   var  f, g ;
   74 begin  f := x;  g := y;
   79   while f <> g do
   83   begin
   83     if f < g then g := g-f;
   91     if g < f then f := f-g;
   99   end;
  100   z := f
  101 end;
         74INT    0    5
         75LOD    1    3
         76STO    0    3
         77LOD    1    4
         78STO    0    4
         79LOD    0    3
         80LOD    0    4
         81OPR    0    9
         82JPC    0  100
         83LOD    0    3
         84LOD    0    4
         85OPR    0   10
         86JPC    0   91
         87LOD    0    4
         88LOD    0    3
         89OPR    0    3
         90STO    0    4
         91LOD    0    4
         92LOD    0    3
         93OPR    0   10
         94JPC    0   99
         95LOD    0    3
         96LOD    0    4
         97OPR    0    3
         98STO    0    3
         99JMP    0   79
        100LOD    0    3
        101STO    1    5
        102OPR    0    0
  103 begin 
  104   x := m;  y := n;  call multiply;
  109   x := 25;  y:= 3;  call divide;
  114   x := 84;  y := 36;  call gcd;
  119 end.
        103INT    0    8
        104LIT    0    7
        105STO    0    3
        106LIT    0   85
        107STO    0    4
        108CAL    0    2
        109LIT    0   25
        110STO    0    3
        111LIT    0    3
        112STO    0    4
        113CAL    0   31
        114LIT    0   84
        115STO    0    3
        116LIT    0   36
        117STO    0    4
        118CAL    0   74
        119OPR    0    0
START PL/0
          7
         85
          7
         85
          0
          7
         14
         42
         28
         21
         35
         56
         10
        112
          5
        147
        224
          2
        448
          1
        595
        896
          0
         25
          3
         25
          0
          3
          6
         12
         24
         48
          0
         24
          1
          1
          2
         12
          4
          6
          8
          3
         84
         36
         84
         36
         48
         12
         24
         12
         12
END PL/0
```
## 任务二修改后的PL0源程序
```pascal
const  m = 7, n = 85;
var  x, y, z, q, r;
procedure  multiply;
  var  a, b;
  begin  a := x;  b := y;  z := 0;
    while b > 0 do
    begin
      if odd b then z := z + a;
      a := 2*a ;  b := b/2 ;
    end
  end;
procedure  divide;
  var  w;
  begin  r := x;  q := 0;  w := y;
    while w <= r do w := 2*w ;
    while w > y do
    begin  q := 2*q;  w := w/2;
      if w <= r then
      begin  r := r-w;  q := q+1 end
    end
  end;
procedure  gcd;
  var  f, g ;
begin  f := x;  g := y;
  while f <> g do
  begin
    if f < g then g := g-f;
    if g < f then f := f-g;
  end;
  z := f
end;
begin
  read(x, y);  call multiply; write(z);
  read(x, y);  call divide; write(q);
  read(x, y);  call gcd; write(z);
end.
```
## 任务二输出
```pascal
    0 const  m = 7, n = 85;
    1 var  x, y, z, q, r;
    1 procedure  multiply;
    1   var  a, b;
    2   begin  a := x;  b := y;  z := 0;
    9     while b > 0 do
   13     begin
   13       if odd b then z := z + a;
   20       a := 2*a ;  b := b/2 ;
   28     end
   28   end;
          2INT    0    5
          3LOD    1    3
          4STO    0    3
          5LOD    1    4
          6STO    0    4
          7LIT    0    0
          8STO    1    5
          9LOD    0    4
         10LIT    0    0
         11OPR    0   12
         12JPC    0   29
         13LOD    0    4
         14OPR    0    6
         15JPC    0   20
         16LOD    1    5
         17LOD    0    3
         18OPR    0    2
         19STO    1    5
         20LIT    0    2
         21LOD    0    3
         22OPR    0    4
         23STO    0    3
         24LOD    0    4
         25LIT    0    2
         26OPR    0    5
         27STO    0    4
         28JMP    0    9
         29OPR    0    0
   30 procedure  divide;
   30   var  w;
   31   begin  r := x;  q := 0;  w := y;
   38     while w <= r do w := 2*w ;
   47     while w > y do
   51     begin  q := 2*q;  w := w/2;
   59       if w <= r then
   62       begin  r := r-w;  q := q+1 end
   71     end
   71   end;
         31INT    0    4
         32LOD    1    3
         33STO    1    7
         34LIT    0    0
         35STO    1    6
         36LOD    1    4
         37STO    0    3
         38LOD    0    3
         39LOD    1    7
         40OPR    0   13
         41JPC    0   47
         42LIT    0    2
         43LOD    0    3
         44OPR    0    4
         45STO    0    3
         46JMP    0   38
         47LOD    0    3
         48LOD    1    4
         49OPR    0   12
         50JPC    0   72
         51LIT    0    2
         52LOD    1    6
         53OPR    0    4
         54STO    1    6
         55LOD    0    3
         56LIT    0    2
         57OPR    0    5
         58STO    0    3
         59LOD    0    3
         60LOD    1    7
         61OPR    0   13
         62JPC    0   71
         63LOD    1    7
         64LOD    0    3
         65OPR    0    3
         66STO    1    7
         67LOD    1    6
         68LIT    0    1
         69OPR    0    2
         70STO    1    6
         71JMP    0   47
         72OPR    0    0
   73 procedure  gcd;
   73   var  f, g ;
   74 begin  f := x;  g := y;
   79   while f <> g do
   83   begin
   83     if f < g then g := g-f;
   91     if g < f then f := f-g;
   99   end;
  100   z := f
  101 end;
         74INT    0    5
         75LOD    1    3
         76STO    0    3
         77LOD    1    4
         78STO    0    4
         79LOD    0    3
         80LOD    0    4
         81OPR    0    9
         82JPC    0  100
         83LOD    0    3
         84LOD    0    4
         85OPR    0   10
         86JPC    0   91
         87LOD    0    4
         88LOD    0    3
         89OPR    0    3
         90STO    0    4
         91LOD    0    4
         92LOD    0    3
         93OPR    0   10
         94JPC    0   99
         95LOD    0    3
         96LOD    0    4
         97OPR    0    3
         98STO    0    3
         99JMP    0   79
        100LOD    0    3
        101STO    1    5
        102OPR    0    0
  103 begin
  104   read(x, y);  call multiply; write(z);
  109   read(x, y);  call divide; write(q);
  114   read(x, y);  call gcd; write(z);
  119 end.
        103INT    0    8
        104RD     0    3
        105RD     0    4
        106CAL    0    2
        107LOD    0    5
        108WRT    0    0
        109RD     0    3
        110RD     0    4
        111CAL    0   31
        112LOD    0    6
        113WRT    0    0
        114RD     0    3
        115RD     0    4
        116CAL    0   74
        117LOD    0    5
        118WRT    0    0
        119OPR    0    0
START PL/0
          7
         85
          0
          7
         14
         42
         28
         21
         35
         56
         10
        112
          5
        147
        224
          2
        448
          1
        595
        896
          0
        595
         25
          0
          3
          6
         12
         24
         48
          0
         24
          1
          1
          2
         12
          4
          6
          8
          3
          8
         84
         36
         48
         12
         24
         12
         12
         12
END PL/0
```