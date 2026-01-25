package mds.t2.services;

import mds.t2.entities.*;

import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.List;

public class EventoService {
    private static final List<Evento> listaEventos = new ArrayList<>();
    private static final DateTimeFormatter dateTimeFormatter = DateTimeFormatter.ofPattern("dd-MM-yyyy HH:mm");

    // UC2: Gerir Evento
    public Evento criarEvento(Admin admin, String nome, String descricao, String local, String strInicio, String strFim, int vagas) {
        LocalDateTime dataInicio = LocalDateTime.parse(strInicio, dateTimeFormatter);
        LocalDateTime dataFim = LocalDateTime.parse(strFim, dateTimeFormatter);

        Evento evento =  new Evento(nome, descricao, local, dataInicio, dataFim, vagas);

        listaEventos.add(evento);
        return evento;
    }

    // UC3: Configurar Preços e Opções
    public void adicionarFaseInscricao(Admin admin, Evento evento, String nome, LocalDateTime inicio, LocalDateTime fim, float precoEstudante, float precoNaoEstudante) {
        FaseInscricao faseInscricao = new FaseInscricao(nome, inicio, fim, precoEstudante, precoNaoEstudante);
        evento.getListaFaseInscricao().add(faseInscricao);
    }

    // UC3: Configurar Preços e Opções
    public void adicionarOpcaoAdicional(Admin admin, Evento evento, String nome, float preco, boolean obrigatorio) {
        OpcaoAdicional opcaoAdicional = new OpcaoAdicional(nome, preco, obrigatorio);
        evento.getListaOpcoesAdicionais().add(opcaoAdicional);
    }

    public void listarEventos() {
        System.out.println("\nEVENTOS COM INSCRIÇÕES ABERTAS");
        for (Evento evento : listaEventos) {
            if (evento.getFaseAtiva() != null) {
                System.out.printf("[%d] %-10s | Local: %s%n", evento.getId(), evento.getNome(), evento.getLocal());
            }
        }
    }

    // UC4: Consultar Detalhes do Evento
    public void consultarDetalhesEvento(Evento evento) {
        System.out.println("\n\nDETALHE DO EVENTO: " + evento.getNome().toUpperCase());
        System.out.println("=".repeat(60));
        System.out.println("Descrição: " + evento.getDescricao());
        System.out.println("Local: " + evento.getLocal());
        System.out.println("Vagas Restantes: " + (evento.getMaxVagas() - evento.getListaInscricoes().size()));

        System.out.println("\nTABELA DE PREÇOS:");
        System.out.println("=".repeat(73));
        System.out.printf("| %-15s | %-16s | %-16s | %-13s |%n", "FASE", "INÍCIO", "FIM", "PREÇO");
        System.out.println("-".repeat(73));
        for (FaseInscricao faseInscricao : evento.getListaFaseInscricao()) {
            System.out.printf("| %-15s | %-16s | %-16s | %-5.2f / %-5.2f |%n",
                    faseInscricao.getNomeFase(),
                    faseInscricao.getData_inicio().format(dateTimeFormatter),
                    faseInscricao.getData_fim().format(dateTimeFormatter),
                    faseInscricao.getPreco_estudante(),
                    faseInscricao.getPreco_nao_estudante());
        }
        System.out.println("=".repeat(73));

        System.out.println("\nOPÇÕES ADICIONAIS:");
        System.out.println("=".repeat(42));
        System.out.printf("| %-15s | %-5s | %-12s |%n", "OPÇÃO", "PREÇO", "TIPO");
        System.out.println("-".repeat(42));
        for (OpcaoAdicional opcaoAdicional : evento.getListaOpcoesAdicionais()) {
            String tipo = opcaoAdicional.getObrigatorio() ? "Obrigatório" : "Opcional";
            System.out.printf("| %-15s | %-5.2f | %-12s |%n", opcaoAdicional.getNome(), opcaoAdicional.getPreco(), tipo);
        }
        System.out.println("=".repeat(42));

        System.out.println("\nINSTRUÇÕES DE PAGAMENTO:");
        System.out.println("O pagamento deve ser efetuado via Transferência Bancária.\n");
    }

    // UC8: Consultas e Verificações
    public void consultarListaParticipantes(Evento evento) {
        System.out.println("\n\nLISTA DE PARTICIPANTES DO EVENTO: " + evento.getNome().toUpperCase());
        if(evento.getListaInscricoes().isEmpty()) {
            System.out.println("Não existem inscrições no evento.");
            return;
        }
        List<Inscricao> inscricoes = evento.getListaInscricoes();

        System.out.println("=".repeat(60));
        System.out.printf("| %-3s | %-10s | %-13s | %-8s | %-10s |%n", "ID", "NOME", "TIPO", "VALOR(€)", "PAGAMENTO");
        System.out.println("-".repeat(60));

        for(Inscricao inscricao : inscricoes) {
            int id = inscricao.getId();
            String nome = inscricao.getParticipante().getNome();
            String tipo = inscricao.getEstudante() ? "Estudante" : "Não Estudante";

            float valor = 0.0f;
            String estadoPagamento = "N/A";

            if(inscricao.getPagamento() != null) {
                valor = inscricao.getPagamento().getValor();
                estadoPagamento = inscricao.getPagamento().getConfirmacao_pagamento() ? "PAGO" : "PENDENTE";
            }

            System.out.printf("| %-3d | %-10s | %-13s | %-8.2f | %-10s |%n", id, nome, tipo, valor, estadoPagamento);
        }
        System.out.println("=".repeat(60));
    }

    // UC11: Imprimir/Exportar Listas
    public void exportarListaParticipantes(Evento evento) {
        String nomeFile = "lista_participantes_" + evento.getNome().replace(" ", "_") + ".csv";

        try(FileWriter fileWriter = new FileWriter(nomeFile); PrintWriter printWriter = new PrintWriter(fileWriter)){

            printWriter.println("ID;Nome;Email;Tipo;Valor;Estado Pagamento");

            for(Inscricao inscricao : evento.getListaInscricoes()) {
                String tipo = inscricao.getEstudante() ? "Estudante" : "Não Estudante";
                float valor = inscricao.getPagamento().getValor();
                String estado = inscricao.getPagamento().getConfirmacao_pagamento() ? "Pago" : "Pendente";

                printWriter.printf("%d;%s;%s;%s;%.2f;%s%n",
                        inscricao.getId(), inscricao.getParticipante().getNome(), inscricao.getParticipante().getEmail(), tipo, valor, estado);
            }
            System.out.println("\n\nLista exportada com sucesso para: " + nomeFile);
        } catch(IOException e){
            System.err.println("\n\nErro ao exportar a lista de participantes do evento: " + evento.getNome());
        }
    }
}
