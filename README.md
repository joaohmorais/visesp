# VISESP - Visualizador de Indicadores de Saúde no Estado de São Paulo

O VISESP é um app desenvolvido em R/Shiny, alimentado pelo pacote [rtabnetsp](https://github.com/joaohmorais/rtabnetsp), que possibilita a visualização interativa dos indicadores de saúde da atenção básica no estado de SP. O aplicativo realiza uma raspagem da página [Matriz de Indicadores de Saúde](https://portal.saude.sp.gov.br/links/matriz) da SES, tornando o aplicativo sensível a cada atualização dos indicadores. 

O app está hosteado na plataforma *shinyapps.io*, e pode ser acessado pelo [link](https://joaoamorais.shinyapps.io/visesp/).

# Como Usar

## Tabelas

Para visualizar dados dos indicadores em forma de tabela, selecione a aba Indicadores na barra principal, selecione um indicador clicando no botão roxo à esquerda, e clique na opção Tabela. Nela, você pode filtrar os dados escolhidos por ano, região, ou valor. Você pode também baixar a tabela com os dados escolhidos em formato .csv ou excel.

## Mapas

### Do estado

Para ver mapas coropléticos do estado, selecione a aba Indicadores e escolha um indicador clicando no botão roxo à esquerda. A partir disso, você verá o mapa do estado com o valor dos indicadores. Você pode alternar a visualização entre DRS, Região de Saúde, RRAS ou Município, quando disponíveis. Você pode baixar o mapa clicando no botão Baixar Mapa localizado abaixo dele.

### Por região

Para ver, no entanto, mapas coropléticos de municípios de uma região específica (algum DRS, Região de Saúde ou RRAS), selecione a aba Regiões na barra principal, escolha a região que deseja visualizar no filtro à esquerda e o indicador desejado à direita.

## Gráficos

### Gráficos de linha

Para visualizar gráficos de Linha de um determinado indicador, selecione a aba Indicadores na barra principal, escolha um indicador clicando no botão roxo à esquerda, e selecione a aba Gráficos. Nela, você pode escolher a opção Linhas para visualizar um gráfico de linhas. Selecione as regiões que você deseja no gráfico do lado esquerdo.

### Gráficos de barra

Para visualizar gráficos de Barras de um determinado indicador, selecione a aba Indicadores na barra principal, escolha um indicador clicando no botão roxo à esquerda, e selecione a aba Gráficos. Nela, você pode escolher a opção Barras para visualizar um gráfico de barras. Selecione as regiões que você deseja no gráfico do lado esquerdo.

# Contato

Para dúvidas ou sugestões, contate-me em joao.morais@unifesp.br ou via [twitter](https://twitter.com/joaoamorais).

Agradecimentos especiais à Camila Bertini Martins, orientadora do projeto, Arnaldo Sala, Thaís Konstantyner e Alvaro Fazenda.

Esse projeto R/Shiny fez parte do Trabalho de Conclusão de Curso para Bacharelado em Ciência da Computação, pela Universidade Federal de São Paulo - UNIFESP SJC.
