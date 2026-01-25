# Relatório Eventastic (Implementação Backend)

## Equipa de Desenvolvimento
- Miguel Grilo | 58387 | IACD
- Tiago Ramalho | 58514 | EI

<br>

## 1. Introdução

Este projeto corresponde à 2ª Parte do Trabalho Prático de MDS, focando-se na implementação do backend para o sistema de gestão de eventos Eventastic. O sistema foi desenvolvido como uma biblioteca Java, desenhada para ser importada e utilizada por outras aplicações.

O objetivo principal foi concretizar os Use Cases e o Diagrama de Classes definidos na fase anterior, adaptando-os para um ambiente de persistência em memória.



## 2. Decisões de Implementação e Arquitetura

### 2.1 Estrutura do Projeto

A aplicação foi estruturada seguindo o padrão de separação de responsabilidades, dividindo o código em duas pastas principais:
- `mds.t2.entities`: Contém as entidades que representam os dados (ex: `Evento`, `Participante`, `Inscricao`, ...).
- `mds.t2.services`: Contém a lógica do sistema. As classes `EventoService` e `InscricaoService` atuam como "controladores" da biblioteca, expondo os métodos públicos que aplicações externas devem invocar.

### 2.2 Persistência de Dados

Conforme o requisito de não utilizar bases de dados:
- **Armazenamento**: A persistência é simulada através de estruturas de dados em memória (Listas). A classe `EventoService` mantém uma `static final List<Evento>`, atuando como o repositório central durante o ciclo de vida da execução.
- **Geração de IDs**: Para simular chaves primárias de uma base de dados, foram utilizados atributos estáticos nas classes `Utilizador`, `Evento` e `Inscricao`. Isto garante que cada nova entidade criada receba um ID único e incremental automaticamente.

### 2.3 Gestão de Utilizadores e Herança
Foi implementada a classe abstrata `Utilizador` para agrupar atributos comuns (`id`, `email`, `username`, `password`).

- **`Admin` e `Participante`**: Ambas estendem `Utilizador`. Embora a autenticação não tenha sido implementada (conforme restrição do enunciado), esta estrutura facilita a escalabilidade futura caso seja necessário distinguir permissões ou adicionar login.

### 2.4 Lógica e Serviços
A lógica complexa foi retirada das entidades e centralizada nos serviços:
- **Cálculo de Preços**: O método `calcularValorTotal` em `InscricaoService` determina dinamicamente o custo da inscrição verificando se o utilizador é estudante, a fase de inscrição ativa (`FaseInscricao`) e as opções adicionais selecionadas.
- **Gestão de Fases**: O sistema verifica automaticamente a fase ativa de um evento comparando a `LocalDateTime.now()` com as datas de início e fim das fases configuradas.

### 2.5 Tratamento de Datas e Pagamentos
- **Datas**: Utilizou-se `LocalDateTime` para toda a gestão temporal. Para facilitar a introdução de dados e visualização, foi definido um `DateTimeFormatter` global (`dd-MM-yyyy HH:mm`).
- **Pagamentos**: O fluxo de pagamento foi simplificado. O estado do pagamento é gerido através do Enum `Estado` (PENDENTE/PAGO) e de uma flag na classe `Pagamento`. O método `efetuarPagamento` altera diretamente este estado, simulando uma confirmação imediata para efeitos de teste, visto que a funcionalidade de confirmar pagamento não deve ser implementada.



## 3. Cobertura de Use Cases (Funcionalidades)
A biblioteca cobre as seguintes funcionalidades principais, mapeadas para os métodos dos serviços:

| Use Case |          Descrição           | Implementação (Service / Método)            |
|:--------:|:----------------------------:|:--------------------------------------------|
| **UC2**  |         Criar Evento         | eventoService.criarEvento()                 |
| **UC3**  |   Configurar Preços/Fases    | eventoService.adicionarFaseInscricao()      |
| **UC3**  | Configurar Opções Adicionais | eventoService.adicionarOpcaoAdicional()     |
| **UC4**  |      Consultar Detalhes      | eventoService.consultarDetalhesEvento()     |
| **UC5**  |      Efetuar Inscrição       | inscricaoService.efetuarInscricao()         |
| **UC6**  |      Efetuar Pagamento       | inscricaoService.efetuarPagamento()         |
| **UC8**  |   Consultar Participantes    | eventoService.consultarListaParticipantes() |
| **UC11** |     Exportar Lista (CSV)     | eventoService.exportarListaParticipantes()  |



## 4. Gestão do Projeto e Ferramentas

### 4.1 Gestão de Dependências e Builds
- O projeto utiliza **Maven** para gestão de dependências e automação de builds.

### 4.2 Controlo de Versões (Git/GitHub)
O desenvolvimento do projeto foi gerido através do Git, com o repositório alojado no GitHub. Foi adotada uma estratégia simplificada para garantir a integridade do código e a rastreabilidade das funcionalidades.

#### 4.2.1 Estratégia de Branches
- `main`: Branch estável que contém o código final de produção após a validação de cada funcionalidade.
- `develop`: Branch de integração onde as novas funcionalidades foram reunidas antes de passarem para a `main`.
- `feature/`: Branches temporárias criadas para o desenvolvimento isolado de componentes específicos (ex: `feature/entities`, `feature/service-uc2`).

#### 4.2.2 Fluxo de Trabalho e Ciclo de Vida
O ciclo de desenvolvimento seguiu um padrão rigoroso de isolamento e integração:
1. Criação de um branch de funcionalidade (feature).
1. Commits granulares (ex: `feat(entities): ...` ou `feat(services): ...`).
1. Push para o GitHub e realização de **Pull Request** para o branch `develop`.
1. Após a consolidação em `develop`, foi realizado o merge para a `main` para marcar a conclusão de marcos estáveis do projeto.

#### 4.2.3 Histórico de Execuções e Commits
|         Fase / Funcionalidade          |   Branch de Origem   | Mensagens de Commit (Resumo)                                                                                                                               | Destino de Integração |
|:--------------------------------------:|:--------------------:|:-----------------------------------------------------------------------------------------------------------------------------------------------------------|:---------------------:|
|           **Entidades Base**           |   feature/entities   | feat(entities): Utilizador, Admin, Participante<br>feat(entities): Pagamento, OpcaoAdicional e FaseInscricao<br>feat(entities): Inscrição, Estado e Evento |        develop        |
|         **Gerir Evento (UC2)**         | feature/service-uc2  | feat(services): Implementação ‘Gerir Evento’<br>feat(main): Testar Funcionalidades UC2                                                                     |    develop -> main    |
|  **Configurar Preços e Opções (UC3)**  | feature/service-uc3  | feat(services): Implementação ‘Configurar Preços e Opções’<br>feat(main): Testar Funcionalidades UC3                                                       |    develop -> main    |
| **Consultar Detalhes do Evento (UC4)** | feature/service-uc4  | feat(services): Implementação ‘Consultar Detalhes do Evento’<br>feat(main): Testar Funcionalidades UC4                                                     |    develop -> main    |
|      **Efetuar Inscrição (UC5)**       | feature/service-uc5  | feat(services): Implementação ‘Efetuar Inscrição’<br>feat(main): Testar Funcionalidades UC5 e CalcularValorTotal                                           |    develop -> main    |
|      **Efetuar Pagamento (UC6)**       | feature/service-uc6  | feat(services): Implementação ‘Efetuar Pagamento’<br>feat(main): Testar Funcionalidades UC6                                                                |    develop -> main    |
|   **Consultas e Verificações (UC8)**   | feature/service-uc8  | feat(services): Implementação ‘Consultas e Verificações’<br>feat(main): Testar Funcionalidades UC8                                                         |    develop -> main    |
|  **Imprimir/Exportar Listas (UC11)**   | feature/service-uc11 | feat(services): Implementação ‘Imprimir/Exportar Listas’<br>feat(main): Testar Funcionalidades UC11                                                        |    develop -> main    |

#### 4.2.4 Gestão de Tarefas (Issues)
Cada funcionalidade (Use Case) ou grupo de entidades foi mapeado para uma Issue no GitHub. Isto permitiu associar cada Pull Request a uma tarefa específica, garantindo que todos os requisitos do enunciado fossem implementados de forma organizada e sequencial.



## 5. Funcionamento do Main

### 5.1 Fluxo de Execução Demonstrado
A classe Main executa sequencialmente as seguintes operações:

1. **Instanciação dos Serviços**: Inicialização do EventoService e InscricaoService, que gerem a lógica.

1. **Gerir Evento (UC2)**: Criação de um perfil de Administrador e um evento base ("Eventastic") localizado em Évora.

1. **Configurar Preços e Opções (UC3)**:
   - **Configuração de três fases de preço**: Early, Late e Durante (com preços diferenciados para estudantes).
   - **Adição de opções adicionais**: "Almoço" (obrigatório), "Lanche", "T-shirt" e "Porta chaves".

1. **Consultar Detalhes do Evento (UC4)**: Listagem de todos os eventos no sistema e detalhe específico do evento criado.

1. **Efetuar Inscrições (UC5)**: Registo de dois participantes distintos:
   - **Miguel**: Inscrição como não-estudante.
   - **Tiago**: Inscrição como estudante.

1. **Efetuar Pagamentos (UC6)**: Simulação da liquidação da inscrição do participante Miguel.

1. **Consultas e Verificações (UC8)**: Exibição da lista de participantes e respetivos estados de pagamento na consola.

1. **Imprimir/Exportar Listas (UC11)**: Gera automaticamente um ficheiro .csv com os dados consolidados.

### 5.2 Compilar e Executar
Para testar, basta compilar o projeto e executar a classe Main. Após a execução, será gerado na raiz do projeto um ficheiro .csv com a lista de participantes do evento criado.

```bash
    # Compilar
    mvn compile
    # Executar
    mvn exec:java -Dexec.mainClass=mds.t2.Main
```
