package mds.t2.entities;

import java.time.LocalDateTime;

public class FaseInscricao {
    private String nomeFase;
    private LocalDateTime data_inicio;
    private LocalDateTime data_fim;
    private float preco_estudante;
    private float preco_nao_estudante;

    public FaseInscricao(String nomeFase, LocalDateTime data_inicio, LocalDateTime data_fim, float preco_estudante, float preco_nao_estudante){
        this.nomeFase=nomeFase;
        this.data_inicio=data_inicio;
        this.data_fim=data_fim;
        this.preco_estudante=preco_estudante;
        this.preco_nao_estudante=preco_nao_estudante;
    }

    public String getNomeFase(){ return nomeFase; }
    public void setNomeFase(String nomeFase){ this.nomeFase = nomeFase; }

    public LocalDateTime getData_inicio(){ return data_inicio; }
    public void setData_inicio(LocalDateTime data_inicio){ this.data_inicio=data_inicio; }

    public LocalDateTime getData_fim(){ return data_fim; }
    public void setData_fim(LocalDateTime data_fim){ this.data_fim=data_fim; }

    public float getPreco_estudante(){ return preco_estudante; }
    public void setPreco_estudante(float preco_estudante){ this.preco_estudante=preco_estudante; }

    public float getPreco_nao_estudante(){ return preco_nao_estudante; }
    public void setPreco_nao_estudante(float preco_nao_estudante){ this.preco_nao_estudante=preco_nao_estudante; }
}