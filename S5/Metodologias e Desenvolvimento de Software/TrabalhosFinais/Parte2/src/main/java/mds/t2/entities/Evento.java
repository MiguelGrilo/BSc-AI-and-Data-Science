package mds.t2.entities;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

public class Evento {
    private int id;
    private static int contadorId = 1;
    private String nome;
    private String descricao;
    private String local;
    private LocalDateTime dataInicio;
    private LocalDateTime dataFim;
    private int maxVagas;

    private List<Inscricao>  listaInscricoes;
    private List<Admin>  listaAdmins;
    private List<FaseInscricao>  listaFaseInscricao;
    private List<OpcaoAdicional> listaOpcoesAdicionais;

    public Evento(String nome, String descricao, String local, LocalDateTime dataInicio, LocalDateTime dataFim, int maxVagas){
        this.id = contadorId++;
        this.nome = nome;
        this.descricao = descricao;
        this.local = local;
        this.dataInicio = dataInicio;
        this.dataFim = dataFim;
        this.maxVagas = maxVagas;
        this.listaInscricoes = new ArrayList<>();
        this.listaAdmins = new ArrayList<>();
        this.listaFaseInscricao = new ArrayList<>();
        this.listaOpcoesAdicionais = new ArrayList<>();
    }

    public FaseInscricao getFaseAtiva(){
        for (FaseInscricao faseInscricao : listaFaseInscricao) {
            if (LocalDateTime.now().isBefore(faseInscricao.getData_fim()) && LocalDateTime.now().isAfter(faseInscricao.getData_inicio())) {
                return faseInscricao;
            }
        }
        return null;
    }

    public int getVagasRestantes(){
        return maxVagas - listaInscricoes.size();
    }

    public int getId(){ return id; }
    public void setId(int id){ this.id=id; }

    public String getNome(){ return nome; }
    public void setNome(String nome){ this.nome=nome; }

    public String getDescricao(){ return descricao; }
    public void setDescricao(String descricao){ this.descricao=descricao; }

    public String getLocal(){ return local; }
    public void setLocal(String local){ this.local=local; }

    public LocalDateTime getDataInicio(){ return dataInicio; }
    public void setDataInicio(LocalDateTime dataInicio){ this.dataInicio=dataInicio; }

    public LocalDateTime getDataFim(){ return dataFim; }
    public void setDataFim(LocalDateTime dataFim){ this.dataFim=dataFim; }

    public int getMaxVagas(){ return maxVagas; }
    public void setMaxVagas(int maxVagas){ this.maxVagas=maxVagas; }

    public List<Admin> getListaAdmins(){ return listaAdmins; }
    public void setListaAdmins(List<Admin> listaAdmins){ this.listaAdmins=listaAdmins; }

    public List<Inscricao> getListaInscricoes(){ return listaInscricoes; }
    public void setListaInscricoes(List<Inscricao> listaInscricoes){ this.listaInscricoes=listaInscricoes; }

    public List<FaseInscricao> getListaFaseInscricao(){ return listaFaseInscricao; }
    public void setListaFaseInscricao(List<FaseInscricao> listaFaseInscricao){ this.listaFaseInscricao=listaFaseInscricao; }

    public List<OpcaoAdicional> getListaOpcoesAdicionais(){ return listaOpcoesAdicionais; }
    public void setListaOpcoesAdicionais(List<OpcaoAdicional> listaOpcoesAdicionais){ this.listaOpcoesAdicionais=listaOpcoesAdicionais; }
}