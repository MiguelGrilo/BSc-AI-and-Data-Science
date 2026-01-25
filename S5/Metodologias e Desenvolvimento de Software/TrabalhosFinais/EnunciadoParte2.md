# 2ª parte: Implementação
A segunda parte do trabalho tem como objetivo a implementação do backend para o sistema  Eventastic, tendo como base os use cases e o diagrama de classes desenvolvidos na primeira parte do trabalho. Este backend deve ser implementado como uma biblioteca que possa ser usada invocada por outra aplicação.

No README do projeto devem descrever, em forma de relatório, as decisões tomadas ao longo do trabalho.

As seguintes funcionalidades/restrições não devem ser implementadas:
- Autenticação/login. Assume-se que não é necessário controle de acessos;
- Confirmação do pagamento;
- Check-in dos participantes.

O sistema deve ser implementado numa linguagem de programação à escolha do aluno, como uma biblioteca que possa ser importada e usada por outra aplicação. Para cada funcionalidade/operação deve existir um método que possa ser invocado por outra aplicação. A persistência dos dados deve ser feita usando estruturas de dados comuns existentes (listas, arrays, etc), não recorrendo a qualquer tipo de base de dados.

A implementação deve considerar os seguintes pontos:
- Criar issues no GitHub que devem corresponder às várias tarefas necessárias para a implementação da plataforma por forma a cobrir todas as funcionalidades descritas nos use cases;
- Gestão de versões de todo o projecto usando Git, com commits e pushes regulares para o GitHub do vosso projecto;
- Fazer uma gestão adequada dos branches do projeto;
- Usar um sistema para fazer a gestão de dependências e builds do projeto, por exemplo o Maven;
- Implementação de uma classe "Main" (ou script) com um exemplo de invocação e utilização da biblioteca implementada.