package mds.t2.entities;

public class OpcaoAdicional {
    private String nome;
    private float preco;
    private boolean obrigatorio;

    public OpcaoAdicional(String nome, float preco, boolean obrigatorio){
        this.nome=nome;
        this.preco=preco;
        this.obrigatorio=obrigatorio;
    }

    public String getNome() { return nome; }
    public void setNome(String nome){ this.nome=nome; }

    public float getPreco(){ return preco; }
    public void setPreco(float preco){ this.preco=preco; }

    public boolean getObrigatorio(){ return obrigatorio; }
    public void setObrigatorio(boolean obrigatorio){ this.obrigatorio=obrigatorio; }
}