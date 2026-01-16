(*
Alunos:	58387
	58656
*)

type pos =  (*Tipos de posições no jogo*)
  | Escondido
  | Mina_Escondida
  | Marcado
  | Mina_Marcada
  | Mina_Encontrada
  | Numero of int
  | Vazio;;

type tabuleiro = pos array array;;  (* Tipo de tabuleiro *)
let empty n = (* Cria um tabuleiro vazio (sem minas) de NxN posições *)
  Array.make_matrix n n Escondido;;

let random tabuleiro k =  (*Coloca k minas aleatórias no tabuleiro*)
  let n = Array.length tabuleiro in
  if k < n*n then (*Se a condição for respeitada coloca as minas*)
    for _ = 1 to k do
      let rec mina_random () =
        let a, b = Random.int n, Random.int n in  (*Escolha das coordenadas aleatórias para colocar as minas*)
        if tabuleiro.(a).(b) = Mina_Escondida then mina_random () (*Se a coordenada escolhida já for uma mina volta a colocar*)
        else tabuleiro.(a).(b) <- Mina_Escondida
      in
      mina_random ()
    done
  else  (*Se a condição não for respeitada retorna erro no argumento dado*)
    invalid_arg "Número de minas inválido.";
  tabuleiro;;

let mine tabuleiro r c = (*Coloca uma mina na posição (R,C)*)
  if r <= 0 || c <= 0 || r > Array.length tabuleiro || c > Array.length tabuleiro then  (*Retorna o tabuleiro e fail caso a posição seja inválida no tabuleiro*)
    (tabuleiro, "fail\nPosição Inválida")
  else if tabuleiro.(r-1).(c-1) = Mina_Escondida then (*Se já existir uma mina nessa posição retorna o tabuleiro e mensagem*)
    (tabuleiro, "fail\nJá foi colocada uma mina nesta posição anteriormente")
  else if tabuleiro.(r-1).(c-1) = Escondido then ( (*Coloca uma mina na posição desejada se a posição for Escondido, o pretendido quando queremos inserir uma mina*)
      tabuleiro.(r-1).(c-1) <- Mina_Escondida;
      (tabuleiro, "ok")
  )
  else
    (tabuleiro, "fail\nPosição Inválida");;

let conta_minas tabuleiro r c = (*Conta o número de minas em redor de uma coordenada*)
  let n = Array.length tabuleiro in
  let contador = ref 0 in (*'ref' cria uma referência mutável, para podermos modificar o seu valor com maior facilidade*)
  for a = -1 to 1 do  (*Usamos a e b para somar a uma posição de modo a serem usados como vetor e verificar as posições de forma radial*)
    for b = -1 to 1 do
      if a <> 0 || b <> 0 then
        let x, y = r-1+a, c-1+b in
        if x >= 0 && y >= 0 && x < n && y < n && (tabuleiro.(x).(y) = Mina_Escondida || tabuleiro.(x).(y) = Mina_Marcada || tabuleiro.(x).(y) = Mina_Encontrada) (*Se a posição estiver minada...*)
          then incr contador  (*Aumenta o contador de minas*)
    done
  done;
  !contador;; (*Acede ao valor armazenado por contador*)

let dump tabuleiro = (*Mostra o tabuleiro*)
  let n = Array.length tabuleiro in
  (*Damos print no cabeçalho do jogo para facilitar as jogadas*)
  print_string "\t\t";
  for i=1 to n do
    Printf.printf "%d\t" i  
  done;
  print_newline ();

  Array.iteri (fun i row -> (*Itera nas linhas do tabuleiro*)
    Printf.printf "%d\t(\t" (i+1);  (*Dá print no número da linha*)
    Array.iter (fun pos ->  (*Itera nas colunas do tabuleiro*)
      match pos with
      | Escondido | Mina_Escondida -> Printf.printf "_\t";
      | Vazio -> Printf.printf " \t";
      | Mina_Encontrada -> Printf.printf "+\t";
      | Numero n -> Printf.printf "%d\t" n;
      | Marcado | Mina_Marcada -> Printf.printf "#\t";
    ) row;
    print_endline ")"
  ) tabuleiro;;

let game_over tabuleiro = (*Termina o jogo*)
  print_endline "\nGame Over\nTabuleiro Final:";
  dump tabuleiro;;  (*Printa o tabuleiro final*)
  
let rec sonar tabuleiro r c = (*Função para revelar uma maior área ao invés de revelar posição a posição*)
  let n = Array.length tabuleiro in
  if r >= 0 && c >= 0 && r < n && c < n then (*Se a posição for válida*)
    match tabuleiro.(r).(c) with
    | Escondido ->
        let contador = conta_minas tabuleiro (r+1) (c+1) in  (*Conta minas ao redor acessando todo o tabuleiro e não apenas uma posição específica*)
        tabuleiro.(r).(c) <- if contador = 0 then Vazio else Numero contador; (*Se conta 0 é Vazio, caso contrário é é Numero _*)
        if contador = 0 then 
          mover tabuleiro r c  (*Revela ao redor se não houver minas próximas, ou seja, a posição jogada for vazia*)
        else
          Printf.printf "Na vizinhança da posição (%d, %d) há %d minas\n" (r+1) (c+1) contador (* Exibe o número de minas *)
    | Marcado | Mina_Escondida | Mina_Marcada | Mina_Encontrada | Numero _ | Vazio -> ()  (*Nestes casos não faz nada, não realiza recursão quando chega a estas posições*)
and mover tabuleiro r c = (*Usamos 'and' pois precisamos implementar as funções em simultâneo, visto que se usam uma à outra, cria então uma dependência mutua e a necessidade de se implementarem em simultâneo*)
  for a = -1 to 1 do  (*Usamos a e b para somar a uma posição de modo a serem usados como vetor e verificar as posições de forma radial para cada posição válida na recursão*)
    for b = -1 to 1 do
      let rr = r + a in
      let cc = c + b in
      if (a <> 0 || b <> 0) then sonar tabuleiro rr cc  (*Desde que a posição ainda não tenha sido verificada, ou seja, o vetor não seja nulo, chamamos sonar novamente para a nova posição*)
    done
  done;;
  
let step tabuleiro r c =  (*Realiza uma jogada no tabuleiro*)
  if r <= 0 || c <= 0 || r > Array.length tabuleiro || c > Array.length tabuleiro then (*Verifica se a posição é válida*)
    "Posição inválida. Tente novamente."
  else
    match tabuleiro.(r-1).(c-1) with
    | Mina_Escondida -> (*Termina o jogo*)
        tabuleiro.(r-1).(c-1) <- Mina_Encontrada;
        print_endline "boom";
        game_over tabuleiro;
        "\nBoa sorte para a próxima :')"
    | Marcado | Mina_Marcada ->   (*Não permite realizar a jogada*)
        "Escolheu uma posição marcada. Tente novamente."
    | Escondido ->  (*Revela a posição usando sonar()*)
        sonar tabuleiro (r-1) (c-1);
        "Jogada terminada."
    | _ -> "Esta posição já foi revelada.";;

let mark tabuleiro r c = (*Marca posições que julgamos ser minas*)
  match tabuleiro.(r-1).(c-1) with
  | Escondido -> tabuleiro.(r-1).(c-1) <- Marcado; "ok"
  | Mina_Escondida -> tabuleiro.(r-1).(c-1) <- Mina_Marcada; "ok"
  | _ -> "fail";;
  
let unmark tabuleiro r c =  (*Desmarca posições previamente marcadas*)
  match tabuleiro.(r-1).(c-1) with
  | Marcado -> tabuleiro.(r-1).(c-1) <- Escondido; "ok"
  | Mina_Marcada -> tabuleiro.(r-1).(c-1) <- Mina_Escondida; "ok"
  | _ -> "fail";;

let verificar_done tabuleiro =  (*Verifica vitória ou derrota*)
  let minas_restantes = ref 0 in
  let marcados = ref 0 in
  let minas_marcadas = ref 0 in
  Array.iter (fun row ->  (*Itera sobre cada linha*)
    Array.iter (fun pos ->  (*Itera sobre cada posição/coluna*)
      match pos with
      | Mina_Escondida -> incr minas_restantes
      | Marcado -> incr marcados
      | Mina_Marcada -> incr minas_marcadas
      | _ -> ()
    ) row
  ) tabuleiro;
  if !minas_restantes=0 && !marcados=0 && !minas_marcadas<>0 then "ok" else "fail";; (*Verifica por fim as condições*)

let guardar tabuleiro ficheiro_guardar =  (*Guarda o tabuleiro atual com o nome desejado*)
  let ficheiro = open_out ficheiro_guardar in (*Abre o ficheiro*)
  let n = Array.length tabuleiro in
  Printf.fprintf ficheiro "%d\n" n;  (*Escreve o tamanho do tabuleiro no início do arquivo*)

  Array.iter (fun row ->
    Array.iteri (fun j pos ->
      let char_pos = match pos with (*Escreve cada posição do tabuleiro*)
        | Escondido -> "E"
        | Mina_Escondida -> "M"
        | Marcado -> "X"
        | Mina_Marcada -> "MX"
        | Mina_Encontrada -> "+"
        | Numero v -> string_of_int v
        | Vazio -> "0"
      in
      Printf.fprintf ficheiro "%s" char_pos;
      if j < Array.length row - 1 then Printf.fprintf ficheiro " "  (*Evita um eventual espaço extra no final da linha*)
    ) row;
    output_char ficheiro '\n'
  ) tabuleiro;
  close_out ficheiro; (*Fecha o ficheiro*)
  print_endline "Tabuleiro guardado com sucesso.";;

let carregar ficheiro_carregar =  (*Carrega um tabuleiro de jogo*)
  let ficheiro = open_in ficheiro_carregar in (*Abre o ficheiro*)
  let n = int_of_string (input_line ficheiro) in  (*Lê o tamanho do tabuleiro*)
  let tabuleiro_carregado = Array.make_matrix n n Escondido in
  for i = 0 to n - 1 do (*Lê o estado de cada linha do tabuleiro*)
    let linha = input_line ficheiro in  (*Lê uma linha do ficheiro*)
    let pos = String.split_on_char ' ' linha in (*Divide a linha em posições separadas por um espaço*)
    if Array.length (Array.of_list pos) <> n then (*Verifica se o número de posições é igual ao tamanho*)
      failwith "Número de posições na linha não coincide com o tamanho do tabuleiro";
    Array.iteri (fun j pos -> (*Itera nas posições da linha para atribuir os tipos correspondentes*)
      tabuleiro_carregado.(i).(j) <- match pos with
        | "E" -> Escondido
        | "M" -> Mina_Escondida
        | "X" -> Marcado
        | "MX" -> Mina_Marcada
        | "+" -> Mina_Encontrada
        | "0" -> Vazio
        | _ -> Numero (int_of_string pos)
    ) (Array.of_list pos) (*Converte a lista de posições para um array*)
  done;
  close_in ficheiro;  (*Fecha o ficheiro*)
  print_endline "Tabuleiro carregado com sucesso.";
  tabuleiro_carregado;;

let minesweeper () =
  Random.self_init ();  (* Cria a seed de jogo para garantir que as minas são colocadas aleatóriamente*)
  let tabuleiro_inicio = [||] in (* Inicia o tabuleiro de início *)
  let rec jogo tabuleiro =  (*Função recursiva para o loop de jogo*)
    let input = read_line () in (*Lê o comando digitado pelo user*)
    let ordem = String.split_on_char ' ' input in (*Divide o comando em strings, separadas por espaço no input*)
    match ordem with  (*Processa cada comando digitado pelo user*)
    | ["empty"; n] ->
      let tabuleiro_novo = empty (int_of_string n) in
      print_endline "ok";
      jogo tabuleiro_novo
    | ["random"; k] ->
      let tabuleiro_novo = random tabuleiro (int_of_string k) in
      print_endline "ok";
      jogo tabuleiro_novo
    | ["mine"; r; c] ->
      let (tabuleiro_novo, txt) = mine tabuleiro (int_of_string r) (int_of_string c) in
      print_endline txt;
      jogo tabuleiro_novo
    | ["step"; r; c] ->
      let jogada = step tabuleiro (int_of_string r) (int_of_string c) in
      print_endline jogada;
      if jogada = "\nBoa sorte para a próxima :')" then exit 0;
      jogo tabuleiro
    | ["mark"; r; c] ->
      let marcar = mark tabuleiro (int_of_string r) (int_of_string c) in
      print_endline marcar;
      jogo tabuleiro
    | ["unmark"; r; c] ->
      let desmarcar = unmark tabuleiro (int_of_string r) (int_of_string c) in
      print_endline desmarcar;
      jogo tabuleiro
    | ["done"] ->
      let verificar = verificar_done tabuleiro in
      print_endline verificar
    | ["dump"] ->
      dump tabuleiro;
      jogo tabuleiro
    | ["guardar"; ficheiro_guardar] ->  (*guardar nome_ficheiro.txt*)
      guardar tabuleiro ficheiro_guardar;
      jogo tabuleiro
    | ["carregar"; ficheiro_carregar] ->  (*carregar nome_ficheiro.txt*)
      let tabuleiro_novo = carregar ficheiro_carregar in
      if Array.length tabuleiro_novo = 0 then jogo tabuleiro (* Mantém o tabuleiro atual se o carregamento falhar *)
      else jogo tabuleiro_novo 
    | _ ->
      print_endline "Comando Inválido";
      jogo tabuleiro
  in 
  jogo tabuleiro_inicio (*Inicia o jogo com o tabuleiro_inicio*)

let () = ignore (minesweeper ())  (*Chama a função principal para iniciar o jogo ignorando o seu retorno*)
