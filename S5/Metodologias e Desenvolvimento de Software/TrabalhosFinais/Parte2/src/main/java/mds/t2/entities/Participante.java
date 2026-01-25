package mds.t2.entities;

public class Participante extends Utilizador {
    private String nome;
    private String telefone;
    private String morada;

    public Participante(String email, String username, String password, String nome, String telefone, String morada){
        super(email, username, password);
        this.nome = nome;
        this.telefone = telefone;
        this.morada = morada;
    }

    public String getNome(){ return nome; }
    public void setNome(String nome){ this.nome=nome; }

    public String getTelefone(){ return telefone; }
    public void setTelefone(String telefone){ this.telefone=telefone; }

    public String getMorada(){ return morada; }
    public void setMorada(String morada){ this.morada=morada; }
}