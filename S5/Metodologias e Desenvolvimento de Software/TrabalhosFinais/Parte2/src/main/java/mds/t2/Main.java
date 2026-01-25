package mds.t2;

import mds.t2.entities.*;
import mds.t2.services.EventoService;
import mds.t2.services.InscricaoService;

import java.util.List;

public class Main {
    public static void main(String[] args) {
        eventastic();
    }

    private static void eventastic(){
        EventoService eventoService = new EventoService();
        InscricaoService inscricaoService = new InscricaoService();

        Admin admin = new Admin("Admin", "admin@eventastic.com", "1234");

        // UC2: Gerir Evento
        Evento eventastic = eventoService.criarEvento(
                admin,
                "Eventastic",
                "Evento oficial do trabalho Eventastic",
                "Évora", "29-01-2026 10:00",
                "29-01-2026 18:00",
                50);

        // UC3: Configurar Preços e Opções
        eventoService.adicionarFaseInscricao(admin, eventastic, "Fase Early", eventastic.getDataInicio().minusDays(10), eventastic.getDataInicio().minusDays(2), 10.0f, 15.0f);
        eventoService.adicionarFaseInscricao(admin, eventastic, "Fase Late", eventastic.getDataInicio().minusDays(2), eventastic.getDataInicio(), 11.0f, 16.0f);
        eventoService.adicionarFaseInscricao(admin, eventastic, "Fase Durante", eventastic.getDataInicio(), eventastic.getDataFim(), 12.0f, 17.0f);

        eventoService.adicionarOpcaoAdicional(admin, eventastic, "Almoço", 5.0f, true);
        eventoService.adicionarOpcaoAdicional(admin, eventastic, "Lanche", 2.0f, false);
        eventoService.adicionarOpcaoAdicional(admin, eventastic, "T-shirt", 10.0f, false);
        eventoService.adicionarOpcaoAdicional(admin, eventastic, "Porta chaves", 1.5f, false);

        // UC4: Consultar Detalhes do Evento
        eventoService.listarEventos();
        eventoService.consultarDetalhesEvento(eventastic);

        // UC5: Efetuar Inscrição
        Participante miguel = new Participante("miguel@gmail.com", "MiguelGrilo", "1234", "Miguel", "912345678", "Vila Viçosa");
        Participante tiago = new Participante("Tiago@gmail.com", "TiagoRamalho", "4321", "Tiago", "987654321", "Orvalhos");

        Inscricao inscricaoMiguel = inscricaoService.efetuarInscricao(eventastic, miguel, false, null);
        List<Integer> listaOpcoesAdicionaisTiago = List.of(0, 1, 2, 3);
        Inscricao inscricaoTiago =  inscricaoService.efetuarInscricao(eventastic, tiago, true, listaOpcoesAdicionaisTiago);

        // UC6: Efetuar pagamento
        inscricaoService.efetuarPagamento(inscricaoMiguel);
        //inscricaoService.efetuarPagamento(inscricaoTiago);

        // UC8: Consultas e Verificações
        eventoService.consultarListaParticipantes(eventastic);

        // UC11: Imprimir/Exportar Listas
        eventoService.exportarListaParticipantes(eventastic);
    }
}