package mds.t2.entities;

import java.time.LocalDateTime;

public class Pagamento {
    private String iban;
    private float valor;
    private String descricao_transferencia;
    private LocalDateTime data_transferencia;
    private boolean confirmacao_pagamento;
    private String notas_admin;

    public Pagamento(String iban, float valor){
        this.iban=iban;
        this.valor=valor;
        this.confirmacao_pagamento=false;
    }

    public String getIban(){ return iban; }
    public void setIban(String iban){ this.iban=iban; }

    public float getValor(){ return valor; }
    public void setValor(float valor){ this.valor=valor; }

    public String getDescricao_transferencia(){ return descricao_transferencia; }
    public void setDescricao_transferencia(String descricao_transferencia){this.descricao_transferencia=descricao_transferencia; }

    public LocalDateTime getData_transferencia(){ return data_transferencia; }
    public void setData_transferencia(LocalDateTime data_transferencia){ this.data_transferencia=data_transferencia; }

    public boolean getConfirmacao_pagamento(){ return confirmacao_pagamento; }
    public void setConfirmacao_pagamento(boolean confirmacao_pagamento){ this.confirmacao_pagamento=confirmacao_pagamento; }

    public String getNotas_admin(){  return notas_admin; }
    public void setNotas_admin(String notas_admin){  this.notas_admin=notas_admin; }

}