package mds.t2.entities;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

public class Inscricao{
    private static int contadorGeral = 1;
    private int id;
    private LocalDateTime data_inscricao;
    private boolean estudante;
    private Estado estado;
    private List<OpcaoAdicional> opcoesAdiconais;
    private Participante participante;
    private Evento evento;
    private Pagamento pagamento;

    public Inscricao(boolean estudante, Participante participante, Evento evento){
        this.id=contadorGeral++;
        this.data_inscricao=LocalDateTime.now();
        this.estudante=estudante;
        this.estado=Estado.PENDENTE;
        this.opcoesAdiconais=new ArrayList<>();
        this.participante=participante;
        this.evento=evento;
        this.pagamento=null;

        for(OpcaoAdicional opcaoAdicional : evento.getListaOpcoesAdicionais()){
            if(opcaoAdicional.getObrigatorio()){
                this.opcoesAdiconais.add(opcaoAdicional);
            }
        }
    }

    public int getId(){ return id; }
    public void setId(int id){ this.id=id; }

    public LocalDateTime getData_inscricao(){ return data_inscricao; }
    public void setData_inscricao(LocalDateTime data_inscricao){ this.data_inscricao=data_inscricao; }

    public boolean getEstudante(){ return estudante; }
    public void setEstudante(boolean estudante){ this.estudante=estudante; }

    public List<OpcaoAdicional> getOpcoesAdiconais(){ return opcoesAdiconais; }
    public void setOpcoesAdiconais(List<OpcaoAdicional> opcoesAdiconais){ this.opcoesAdiconais=opcoesAdiconais; }

    public Estado getEstado(){ return estado; }
    public void setEstado(Estado estado){ this.estado=estado; }

    public Participante getParticipante(){ return participante; }
    public void setParticipante(Participante participante){ this.participante=participante; }

    public Evento getEvento(){ return evento; }
    public void setEvento(Evento evento){ this.evento=evento; }

    public Pagamento getPagamento(){ return pagamento; }
    public void setPagamento(Pagamento pagamento){ this.pagamento=pagamento; }
}