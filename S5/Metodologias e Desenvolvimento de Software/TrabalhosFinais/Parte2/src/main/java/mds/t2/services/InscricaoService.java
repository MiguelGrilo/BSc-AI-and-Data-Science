package mds.t2.services;

import mds.t2.entities.*;

import java.util.List;

public class InscricaoService {

    public float calcularValorTotal(Inscricao inscricao, FaseInscricao faseInscricao) {
        float preco = 0;
        if (inscricao.getEstudante()) {
            preco += faseInscricao.getPreco_estudante();
        } else {
            preco += faseInscricao.getPreco_nao_estudante();
        }

        List<OpcaoAdicional> listaOpcoesAdicionais = inscricao.getOpcoesAdiconais();
        if (!listaOpcoesAdicionais.isEmpty()) {
            for (OpcaoAdicional opcaoAdicional : listaOpcoesAdicionais) {
                preco += opcaoAdicional.getPreco();
            }
        }
        return preco;
    }

    // UC5: Efetuar Inscrição
    public Inscricao efetuarInscricao(Evento evento, Participante participante, boolean estudante, List<Integer> escolhaOpcoesAdicionais) {
        if (evento.getVagasRestantes() <= 0) {
            System.out.println("\nNão existem vagas para o evento: " + evento.getNome());
            return null;
        } else if (evento.getFaseAtiva() == null) {
            System.out.println("\nNão é possível realizar inscrições no evento: " + evento.getNome());
            return null;
        }

        Inscricao inscricao = new Inscricao(estudante, participante, evento);
        if (escolhaOpcoesAdicionais != null && !escolhaOpcoesAdicionais.isEmpty()) {
            for (Integer i : escolhaOpcoesAdicionais) {
                if (evento.getListaOpcoesAdicionais().size() <= i || i < 0) {
                    continue;
                }
                OpcaoAdicional opcaoEscolhida = evento.getListaOpcoesAdicionais().get(i);
                if (!inscricao.getOpcoesAdiconais().contains(opcaoEscolhida)) {
                    inscricao.getOpcoesAdiconais().add(opcaoEscolhida);
                }
            }
        }
        float preco = calcularValorTotal(inscricao, evento.getFaseAtiva());

        String iban = "PT50 0000 4521 0453 05";
        inscricao.setPagamento(new Pagamento(iban, preco));

        System.out.println("\nDADOS DE PAGAMENTO '" + participante.getNome() + "'");
        System.out.println("Iban: " + iban + "\nPreço: " + preco + "€");
        evento.getListaInscricoes().add(inscricao);
        return inscricao;
    }

    // UC6: Efetuar Pagamento
    public void efetuarPagamento(Inscricao inscricao){
        if(inscricao == null){
            System.out.println("\nNão é possível prosseguir com o pagamento.");
            return;
        }
        inscricao.setEstado(Estado.PAGO);
        inscricao.getPagamento().setConfirmacao_pagamento(true);
        System.out.printf("\nO pagamento do participante %s foi efetuado", inscricao.getParticipante().getNome());
    }
}