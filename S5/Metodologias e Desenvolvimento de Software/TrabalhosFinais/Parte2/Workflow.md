# Workflow Eventastic (Implementação Backend)

Este repositório contém a implementação do projeto **Eventastic**. O foco deste documento é detalhar o **Fluxo de Trabalho (Workflow)**, a **Estratégia de Branching** e as **Práticas de Versionamento** utilizadas durante o desenvolvimento, simulando um ambiente profissional de Engenharia de Software.

---

## Metodologia de Desenvolvimento

O projeto segue uma adaptação rigorosa do **Gitflow**. O ciclo de vida do código passou por validação através de **Pull Requests (PR)** no GitHub antes da integração.

### Estratégia de Branching
A árvore do projeto está estruturada em três níveis de estabilidade:

1.  **`main`**: Código de produção. Recebe apenas merges da `develop` após validação completa.
2.  **`develop`**: Branch de integração. Todas as funcionalidades convergem aqui.
3.  **`feature/*`**: Branches temporárias para desenvolvimento de Use Cases específicos ou entidades.

```mermaid
gitGraph
   commit id: "init"
   branch develop
   checkout develop
   commit id: "initial push"
   
   %% --- ENTIDADES ---
   branch feature/entities
   checkout feature/entities
   commit id: "feat: (#1)"
   commit id: "feat: (#2)"
   commit id: "feat: (#3)"
   checkout develop
   merge feature/entities id: "PR&M - entities into dev"

   %% --- UC2: Gerir Evento ---
   branch feature/service-uc2
   checkout feature/service-uc2
   commit id: "feat: uc2 (#4)"
   commit id: "feat: teste (#4)"
   checkout develop
   merge feature/service-uc2 id: "PR&M - uc2 into dev"
   checkout main
   merge develop id: "PR&M - dev into main (#4)"
   checkout develop

   %% --- UC3: Preços e Opções ---
   branch feature/service-uc3
   checkout feature/service-uc3
   commit id: "feat: uc3 (#8)"
   commit id: "feat: teste (#8)"
   checkout develop
   merge feature/service-uc3 id: "PR&M - uc3 into dev"
   checkout main
   merge develop id: "PR&M - dev into main (#8)"
   checkout develop

   %% --- UC4: Detalhes ---
   branch feature/service-uc4
   checkout feature/service-uc4
   commit id: "feat: uc4 (#11)"
   commit id: "feat: teste (#11)"
   checkout develop
   merge feature/service-uc4 id: "PR&M - uc4 into dev"
   checkout main
   merge develop id: "PR&M - dev into main (#11)"
   checkout develop

   %% --- UC5: Inscrição ---
   branch feature/service-uc5
   checkout feature/service-uc5
   commit id: "feat: uc5 (#14)"
   commit id: "feat: teste (#14)"
   checkout develop
   merge feature/service-uc5 id: "PR&M - uc5 into dev"
   checkout main
   merge develop id: "PR&M - dev into main (#14)"
   checkout develop

   %% --- UC6: Pagamento ---
   branch feature/service-uc6
   checkout feature/service-uc6
   commit id: "feat: uc6 (#17)"
   commit id: "feat: teste (#17)"
   checkout develop
   merge feature/service-uc6 id: "PR&M - uc6 into dev"
   checkout main
   merge develop id: "PR&M - dev into main (#17)"
   checkout develop

   %% --- UC8: Consultas ---
   branch feature/service-uc8
   checkout feature/service-uc8
   commit id: "feat: uc8 (#20)"
   commit id: "feat: teste (#20)"
   checkout develop
   merge feature/service-uc8 id: "PR&M - uc8 into dev"
   checkout main
   merge develop id: "PR&M - dev into main (#20)"
   checkout develop

   %% --- UC11: Listas ---
   branch feature/service-uc11
   checkout feature/service-uc11
   commit id: "feat: uc11 (#23)"
   commit id: "feat: teste (#23)"
   checkout develop
   merge feature/service-uc11 id: "PR&M - uc11 into dev"
   checkout main
   merge develop id: "PR&M - dev into main (#23)"
   ```

### Lista de Issues
O planeamento foi realizado através de GitHub Issues, categorizadas com a label feature para cada Use Case.

| ID | Tipo | Descrição / Caso de Uso (UC) | Estado |
| :--- | :--- | :--- | :--- |
| **#1** | Entidade | Implementar 'Utilizador, Admin, Participante' | Closed |
| **#2** | Entidade | Implementar 'Pagamento, OpcaoAdicional e FaseInscricao' | Closed |
| **#3** | Entidade | Implementar 'Inscrição, Estado e Evento' | Closed |
| **#4** | Serviço | UC2: Implementar ‘Gerir Evento’ | Closed |
| **#8** | Serviço | UC3: Implementar ‘Configurar Preços e Opções’ | Closed |
| **#11** | Serviço | UC4: Implementar ‘Consultar Detalhes do Evento’ | Closed |
| **#14** | Serviço | UC5: Implementar ‘Efetuar Inscrição’ | Closed |
| **#17** | Serviço | UC6: Implementar ‘Efetuar Pagamento’ | Closed |
| **#20** | Serviço | UC8: Implementar ‘Consultas e Verificações’ | Closed |
| **#23** | Serviço | UC11: Implementar ‘Imprimir/Exportar Listas’ | Closed |


### Padrão de Commits
#### Estrutura: 
- tipo(contexto): descrição (ref #issue)
- feat: Nova funcionalidade.

#### Exemplos Reais do Histórico
- feat(entities): Utilizador, Admin, Participante (close #1)
- feat(services): Implementação ‘Gerir Evento’ (ref #4)
- feat(main): Testar Funcionalidades UC11 (close #23)

### Workflow Detalhado (Passo a Passo)
Para cada funcionalidade implementada, foi seguido o seguinte ciclo:
#### 1. Inicialização
Partindo da branch develop atualizada, cria-se uma branch específica para a tarefa:

```bash
git checkout develop
git pull origin develop
git checkout -b feature/service-uc11
```
#### 2. Desenvolvimento & Commit
Implementação da lógica e respetivos testes na classe Main.

```bash
git add .
git commit -m "feat(services): Implementação ‘Imprimir/Exportar Listas’ (ref #23)"
# ...
git commit -m "feat(main): Testar Funcionalidades UC11 (close #23)"
```
#### 3. Pull Request (GitHub)
O merge não é realizado localmente. É feito o push da feature branch e aberto um Pull Request para a develop.
#### 4. Integração (Merge)
- Feature -> Develop: Após revisão, o PR é aceite e a feature entra na develop.
- Develop -> Main: Após implementação de cada Use Case abre-se um PR da develop para a main para consolidar a versão estável.
