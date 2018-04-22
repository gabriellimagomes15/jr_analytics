/**********************************************************************************
******************************** VARIAVEIS GLOBAIS ********************************/

// OBJETO PARA ARMAZENAR OS DADOS CARREGADOS
var dados = {};
//VARIAVIES PARA EFEITOS DE DELAY
var delay = 2000;
var duration = 1500;

var limitSelec = 1;

/**************************************************************************************************************************************************************************
**************************************************************************************************************************************************************************



/**********************************************************************************
***********************************************************************************/


/**********************************************************************************
******************************** FUNÇÕES ********************************/

//FUNÇÃO PARA LER OS DADOS. OS DADOS SERÃO ARMAZENADOS EM UM OBJETO CHAMADO dados
 // PARAMETROS: paths(caminho do arquivo); names(nome da chave onde serão armazenados os dados. )
function readData (path, names){
	console.log(path,names)
	d3.csv(path, function(error, data){
  		dados[names] = data;
  	});
}

function createNestingFunction(propertyName){
	return function(d){ 
    	return d[propertyName];
    };
}


/** FUNÇÃO PARA FAZER GROUPBY DINAMICAMENTE. 
PARAMETROS: dados-ARRAY COM OS DADOS A SEREM AGRUPADOS; levels-CAMPOS DO ARRAY QUE SERÃO AGRUPADOS **/
function group(dados,levels){
	// CRIANDO OBJETO DO TIPO 'nest()'
	var nest = d3.nest();

	//LOOP PARA CRIAR AS 'KEYs' DINAMICAMENTE PARA O GROUPBY
	for (var i = 0; i < levels.length; i++) {        
        nest = nest.key( createNestingFunction(levels[i]) );        
	}
	var groupBy = nest.rollup(function(v) { 
		return v.length; 
	}).entries(dados);
	
	groupBy = JSON.parse(JSON.stringify(groupBy).split('"value":').join('"count":'));//ALTERAR NOME DOS ATRIBUTOS DO JSON
	groupBy = JSON.parse(JSON.stringify(groupBy).split('"key":').join('"value":')); //ALTERAR NOME DOS ATRIBUTOS DO JSON

    return groupBy;
}

//FUNÇÃO PARA FAZER GROUPBY COM MÉTRICAS ESTATÍSTICAS EM CAMPOS NUMÉRICOS
 //fieldGroup: campo que será feito o groupby
 //fieldMetric: campo que será feitos os cálculos (este campo deve ser numérico)
function groupMetric(data, fieldGroup, fieldMetric){
	var nest = d3.nest()
				 .key( function(x) {return x[fieldGroup]} )
				 .rollup(function(v) {
							return {
									count: v.length,
									sum: d3.sum(v, function(d) { return d[fieldMetric]}),
									avg: d3.mean(v, function(d) {return d[fieldMetric]}),
									min: d3.min(v, function(d){ return d[fieldMetric]}),
									max: d3.max(v, function(d){ return d[fieldMetric]})
								} 
						})
				 .entries(data);
	
	nest = JSON.parse(JSON.stringify(nest).split('"value":').join('"count":'));//ALTERAR NOME DOS ATRIBUTOS DO JSON
	nest = JSON.parse(JSON.stringify(nest).split('"key":').join('"value":')); //ALTERAR NOME DOS ATRIBUTOS DO JSON

    return nest;
}


//FUNÇÃO PARA DESENHAR OS EIXOS X e Y
function drawAxix(params){	
	this.append("g")
		.attr("class", "axis axis--x")
		.attr("transform", "translate(0," + params.margins.margins.height + ")")//params.margins.margins.height
		.transition()
		.delay(delay)
		.duration(duration)
		.call(d3.axisBottom(params.axis.x))			
  	.selectAll("text")
		.style("text-anchor", "end")
		.style("font-size", "11px")
		.attr("dx", "-.8em")
		.attr("dy", ".8em")
		.attr("transform", "translate(0,0) rotate(-40)" ); //

  	this.append("g")
		.attr("class", "axis axis--y")		
		.transition()
		.delay(delay)
		.duration(duration)
		.call(d3.axisLeft(params.axis.y).ticks(10,"s"))//params.axis.
}


function barChart(params){
	//var color = d3.scaleOrdinal(d3.schemeCategory20c);
	var scaleColor = d3.scaleSequential(d3["interpolateBlues"]).domain([0, d3.max(params.data, function(d) { return d.count; })]);

	var bar = this.selectAll(".bar")
			  	.data(params.data)
			  	.enter().append("rect")
			  	.classed("hover", true) //CLASSE PARA ATRIBUIR O EFEITO DE MOUSE-OVER
			  	.classed("bar", params.setFill)

	// IF PARA PLOTAR GRÁFICO HORIZONAL ou VERTICAL
	if(params.horizontal){
		params.axis.y.domain(params.data.map(function(d) { return d.value; }));
		params.axis.x.domain([0, d3.max(params.data, function(d) { return d.count; })]);

		bar.attr("x", 0)
			.transition()
				.delay(function(d, i) {
					return i * 100;
				})
			.duration(duration)
			.attr("y", function(d) { return params.axis.y(d.value); })
			.attr("height", params.axis.y.bandwidth() )
			.attr("width", function(d) { return params.axis.x(d.count)})
			.attr("fill", function(d,i){ return scaleColor(d.count)} )
		

	}else{
		params.axis.x.domain(params.data.map(function(d) { return d.value; }));
		params.axis.y.domain([0, d3.max(params.data, function(d) { return d.count; })]);
	
		bar.transition()
			.delay(function(d, i) {
				return i * 100;
			})
			.duration(duration)
			.attr("x", function(d) { return params.axis.x(d.value); })
			.attr("y", function(d) { return params.axis.y(d.count); })
			.attr("width", params.axis.x.bandwidth())
			.attr("height", function(d) { return params.margins.margins.height - params.axis.y(d.count); })//params.margins.margins.margin.height
			.attr("fill", function(d,i){ return scaleColor(d.count)} )

	}
	bar.on("mousemove", function(d){
			tooltip
				.style("left", d3.event.pageX - 50 + "px")
				.style("top", d3.event.pageY - 70 + "px")
				.style("display", "inline-block")
				.html((d.value) + "<br>" + (d.count.toLocaleString('de-DE')));
				//.html('teste tooltip');
		})
		.on("mouseout", function(d){ tooltip.style("display", "none");});

    //DESENHANDO OS LABELS NOS EIXOS X e Y
	drawAxix.call(this, params)
}

//FUNÇÃO PARA CRIAR GRÁFICO DE PIZZA
function pieChar(params){
	var data    = params.data;
	var text    = "";
	var width   = params.margins.margins.width;
	var height  = params.margins.margins.height;
	var opacity = .65;
	var opacityHover = 1;
	var otherOpacityOnHover = opacity;//.7;
	//	var duration = 500;
	var delayPie = (delay * 0.3); //PEGANDO % DO DELAY GLOBAL

	var radius = Math.min(width, height) / 1.8;
	var color  = d3.scaleOrdinal(d3.schemeCategory10);
		var scaleColor = d3.scaleSequential(d3["interpolateBlues"])
					   .domain([0, d3.max(data, function(d) { return d.count; })]);

	this.attr('transform', 'translate(' + (width/2.5) + ',' + (height/1.5) + ')')

	var arc = d3.arc()
	            .innerRadius(0)
	            .outerRadius(radius);

    //VARIAVEL PARAR O RAIO DO ARCO DA GRAFICO PIZZA
	var arcOver = d3.arc()
				    .innerRadius(0)
					.outerRadius(radius + 10);

	var pie = d3.pie()
	            .value(function(d) { return d.count; })
	            .sort(null);

	var path = this.selectAll('path')
		.data(pie(data))
		.enter()
		.append("g")
		.append('path')
		.classed("pie", true)
		.attr('fill', function(d, i){ return color(d.data.count);} )
		.transition()
		.delay(function(d,i){
			return i * delayPie;
		})
		.duration(duration)
		.attr('d', arc)
		.style('opacity', opacity)
		.style('stroke', 'white')

		
	d3.selectAll('.pie')
		.on("mousemove", function(d){
			d3.select(this)
				.style("opacity", otherOpacityOnHover);

			d3.select(this)
				.style("opacity", opacityHover)
				.attr('d', arcOver)
			tooltip
				.style("left", d3.event.pageX - 50 + "px")
				.style("top", d3.event.pageY - 70 + "px")
				.style("display", "inline-block")				
				.html(d.data.value + '<br>' + d.data.count.toLocaleString('de-DE') )
		})
		.on("mouseout", function(d){
			d3.select(this)
			    .style("opacity", opacity)
			    .attr('d', arc);

			tooltip.style("display", "none");
		})
		.on("touchstart", function(d) {
		  d3.select("svg")
		    .style("cursor", "none");
		})		
		.each(function(d, i) { this._current = i; });

		//TAMANHO DO RETANGO DA LEGENDA SERÁ 5% DO WIDTH
		var legendRectSize = width * 0.05;
        var legendSpacing = 4;
        var legend = this.selectAll('.legend')                     
						.data(data)                                   
						.enter()                                                
						.append('g')                                            
						.attr('class', 'legend')
						.attr('transform', function(d, i) {                     
							var height = legendRectSize + legendSpacing;          
							var offset = height * color.domain().length / 2;     
							var horz   = width/2.3 - params.margins.margins.margin.right;                       
							var vert   = i * height - offset;                       
							return 'translate(' + horz + ',' + vert + ')';        
						});                                                     

    legend.append('rect')
    	.transition()
		.delay(function(d,i){
			return i * delayPie;
		})
		.duration(duration)
		.attr('width', legendRectSize)                          
		.attr('height', legendRectSize)                         
		.style('fill', function(d) { return color(d.count); })
		
    legend.append('text')
    	.transition()
		.delay(function(d,i){
			return i * delayPie;
		})
		.duration(duration)                
        .attr('x', legendRectSize + legendSpacing)              
        .attr('y', legendRectSize - legendSpacing)              
        .text(function(d) { return d.value + '(' +(d.count.toLocaleString('de-DE')) + ')'; });
}


//FUNÇÃO PARA CRIAR UMA DIV DINAMICAMENTE
 //RECEBE UM OBJETO COMO PARAMETRO, CONTENDO: id, class e título da Div
function createDiv(params){
	var div = d3.select(params.id)
	          .append("div")
	          .attr("class", params.class)
	if(params.titleDiv != ""){
		div.append("h2")
		   .text(params.titleDiv);
	}

	return div;
}


// FUNÇÃO PARA CRIAR SVG DINAMICAMENTE
 //RECEBE UM OBJETO COMO PARAMETRO, CONTENDO: id do SVG, width, height
function createSvg(params){
	var svg = this.append("svg")
				.attr('id', params.idSvg)
	    		.attr("width", params.width)
	    		.attr("height", params.height)    		
    			.attr('preserveAspectRatio','xMinYMin')
	return svg;
}

// FUNÇÃO PARA CRIAR MARGENS DINAMICAMENTE PARA O GRÁFICO
  //RECEBE UM OBJETO COMO PARAMETRO, CONTENDO: valor de margin (top, right, bootom e left)
function createMargins(params){	
	var medidas = {}
    medidas.margin = {top: params.top, right: params.right, bottom: params.bottom, left: params.left};
	medidas.width  = this.attr('width') - medidas.margin.left - medidas.margin.right;
    medidas.height = this.attr('height') - medidas.margin.top - medidas.margin.bottom;

    return medidas;
}


// FUNÇÃO PARA INICIALIZAR VARIAVEIS E OBJETOS PARA CRIAR GRÁFICOS
 //RECEBE UM OBJETO COMO PARAMETRO, CONTENDO: id para a DIV, titulo da DIV, class da DIV,
   //valores de width, height para o SVG, id do SVG
function initVar(params){
	//CRIANDO DIV COM A FUNÇÃO
	var div = createDiv.call(null, {id: params.id, titleDiv: params.titleDiv, class: params.class });

	//CRIANDO SVG COM A FUNÇÃO	
	var svg = createSvg.call(div, {width: params.width, height: params.height, idSvg: params.idSvg});

	//CRIANDO MARGENS PARA O GRÁFICO
	var margins = createMargins.call(svg, {top: 20, right: 20, bottom: 105, left: 60}),
		margin = margins.margin;

	//CRIANDO ELEMENTO 'g'
	var g = svg.append("g").attr("transform", "translate(" + margin.left + "," + margin.top + ")");

	return {g: g, margins : margins};
}


//FUNÇÃO PARA CRIAR BOXES COM INFORMAÇÕES NUMÉRICAS
 //RECEBE UM OBJETO COMO PARAMETRO, CONTENDO: id, class e título da Div
function boxNumber(params){
	var format = d3.format(",d");

	if(params.docs){//BOX COM TOTAL DE DOCUMENTOS
		this.attr('class', 'col-md-3 borderDiv alert alert-info')

		this.append('span').attr('class', "fa fa-database fa-3x").style('float', 'right')

		var h2 = this.append('h2')
					.text(params.title)

		
		this.append('h4')		
			.transition()
            .duration(duration)
            .on("start", function repeat() {
              d3.active(this)
                .tween("text", function() {
                    var that = d3.select(this),
                        i = d3.interpolateNumber(that.text().replace(/,/g, ""), params.docs.length);
                    return function(t) { that.text(format(i(t))); };
                })
                .transition()
                .delay(delay)
            });
			//.text(params.docs.resultCount.toLocaleString('de-DE'))
	}else if(params.inst){ //BOX COM TOTAL DE INSTITUIÇÕES PARTICIPANTES
		
		this.attr('class', 'col-sm-4 borderDiv alert alert-info')
		this.append('span').attr('class', "fa fa-university fa-3x").style('float', 'right') 

		var h2 = this.append('h2').text(params.title)
		
		this.append('h4').transition()
            .duration(duration)
            .on("start", function repeat() {
              d3.active(this)
                .tween("text", function() {
                    var that = d3.select(this),
                        i = d3.interpolateNumber(that.text().replace(/,/g, ""), params.inst.length);
                    return function(t) { that.text(format(i(t))); };
                })
                .transition()
                .delay(delay)
            });
            //.text(params.inst.length.toLocaleString('de-DE'))		
	}	
}

//FUNÇÃO PARA FAZER GRÁFICO DE BOLHAS
 //RECEBE UM OBJETO COMO PARAMETRO, CONTENDO: margins, setColor, data, 
function bubbleChart (params){
	
	var width  = params.margins.width; 
    	height = params.margins.height 

	var pack = d3.pack()
	             .size([width, height])
	             .padding(1.5);

	var color = d3.scaleOrdinal(d3.schemeCategory20c);
	
	if(params.hasOwnProperty("setColor") && params.setColor != ''){
		var scaleColor = d3.scaleSequential(d3[params.setColor]).domain([0, d3.max(params.data, function(d) { return d.count; })]);
	}else{
		var scaleColor = d3.scaleSequential(d3["interpolateBlues"]).domain([0, d3.max(params.data, function(d) { return d.count; })]);
	}
	var opacity = 0.55,
		opacityHover = 1;
	
	var root = d3.hierarchy({children: params.data})
	  .sum(function(d) { return d.count; })
	  .each(function(d) {	  	
	    if (id = d.data.value) {
	      var id, i = id.lastIndexOf(".");
	      d.id      = id;
	    }
	});

	var node = this.selectAll(".node")
					.data(pack(root).leaves())
					.enter().append("g")
					  .attr("class", "node")
					  .attr("transform", function(d) {  return "translate(" + d.x + "," + d.y + ")"; });

	node.append("circle")
	  .attr("id", function(d) { return d.id; })
	  .attr("r", function(d) { return d.r; })
	  .style("fill", function(d) {  return scaleColor(d.data.count); })
	  .style('opacity', opacity);

	node.append("clipPath")
	  .attr("id", function(d) { return "clip-" + d.id; })
	.append("use")
	  .attr("xlink:href", function(d) { return "#" + d.id; });

	node.append("text")
	  .attr("clip-path", function(d) { return "url(#clip-" + d.id + ")"; })
	.selectAll("tspan")
	.data(function(d) { return d.id.split(/(?=[A-Z][^A-Z])/g) /*d.class.split(/(?=[A-Z][^A-Z])/g)*/; })
	.enter().append("tspan")
	  .attr("x", 0)
	  .attr("y", function(d, i, nodes) { return 13 + (i - nodes.length / 2 - 0.5) * 10; })
	  .text(function(d) { return d; });

	node.on("mousemove", function(d){
		d3.select(this).select('circle').transition().delay(0).duration(100)
			.attr("r", function(d) { return d.r + 10; }) //.style("fill", 'red');
			.style("opacity", opacityHover)
			.style('fill', "brown")
	        
	        tooltip
	          .style("left", d3.event.pageX - 50 + "px")
	          .style("top", d3.event.pageY - 70 + "px")
	          .style("display", "inline-block")	          
	          .html((d.id) + "<br>" + (d.data.count.toLocaleString('de-DE')));
	})
	.on("mouseout", function(d){ 
		d3.select(this).select('circle').transition().delay(0).duration(100)
				.attr("r", function(d) { return d.r; })
				.style("fill",  function(d) { return scaleColor(d.data.count);} )
				.style('opacity', opacity);

		tooltip.style("display", "none");
	});
}

//FUNÇÃO PARA FAZER GRÁFICO DE LINHA
 //RECEBE UM OBJETO COMO PARAMETRO, CONTENDO: margins, data,
function lineChart(params){
	var width  = params.margins.width,
		height = params.margins.height,
		data   = params.data;

	var x = d3.scaleTime().rangeRound([0, width]);

	var y = d3.scaleLinear().rangeRound([height, 0]);

	var line = d3.line().x(function(d) { return x(d.value); }).y(function(d) { return y(d.count); });

	x.domain(d3.extent(data, function(d) { return d.value; }));
	y.domain(d3.extent(data, function(d) { return d.count; }));

	this.append("g")
		.attr("class", "x-axis")
		.attr("transform", "translate(0," + height + ")")
		.call(d3.axisBottom(x))
		.selectAll("text")
		.style("text-anchor", "end")
		.style("font-size", "11px")
		.attr("dx", "-.8em")
		.attr("dy", ".8em")
		.attr("transform", "translate(0,0) rotate(-40)" ); 

	this.append("g")    			  
			  .attr("class", "y-axis")
		      .call(d3.axisLeft(y)
		        .ticks(5)
		        .tickFormat(d3.format(".2s")))

	this.append("text")
	  .attr("fill", "#000")
	  .attr("transform", "rotate(-90)")
	  .attr("y", 6)
	  .attr("dy", "0.71em")
	  .attr("text-anchor", "end")

	this.append("path").attr('class','line')
		.datum(data)
		.attr("fill", "none")
		.attr("stroke", "steelblue")
		.attr("stroke-linejoin", "round")
		.attr("stroke-linecap", "round")
		.attr("stroke-width", 1.5)
		.attr("d", line)
	
	var format = d3.timeFormat("%d-%m-%Y")

	this.selectAll("dot")	
        .data(data)			
    .enter().append("circle")								
        .attr("r", 5)		
        .attr("cx", function(d) { return x(d.value); })		 
        .attr("cy", function(d) { return y(d.count); })
        .style("opacity", .5)
        .on("mouseover", function(d) {
        	d3.select(this).transition()		
                .duration(200)		
                .style("opacity", 1)
                .style('fill', 'red');
            tooltip
            .html(format(d.value) + "<br/>"  + d.count)	
                .style("left", (d3.event.pageX) + "px")		
                .style("top", (d3.event.pageY - 28) + "px")
                .style("display", "inline-block")
            })					
        .on("mouseout", function(d) {		
            d3.select(this).style("opacity", .5)
            .style('fill', '').transition()		
                .duration(200);
            tooltip.style("display", "none")
        });
}

//FUNÇÃO PARA FAZER GRÁFICO TREE MAP
 //RECEBE UM OBJETO COMO PARAMETRO, CONTENDO: margins, data,
function treeMap(params){
	var data = params.data,
	  width  = params.margins.width,
	  height = params.margins.height;

	var format = d3.format(",d");
		
	var scaleColor = d3.scaleSequential(d3["interpolateBlues"])
					   .domain([0, d3.max(params.data, function(d) { return d.count; })]);

	var treemap = d3.treemap()
	  .tile(d3.treemapResquarify)
	  .size([width, height])
	  .round(true)
	  .paddingInner(1);

	 var root = d3.hierarchy({children: data})
	  .sum(function(d) { return d.count; })
	  .each(function(d) {	  	
	    if (id = d.data.value) {
	      var id, i = id.lastIndexOf(".");
	      d.id      = id;
	  }
	});

    treemap(root);

    var cell = this.selectAll("g")
      .data(root.leaves())
      .enter().append("g")
        .attr("transform", function(d) { return "translate(" + d.x0 + "," + d.y0 + ")"; });

    cell.append("rect")
        .attr("id", function(d) { return d.id; })
        .attr("width", function(d) { return d.x1 - d.x0; })
        .attr("height", function(d) { return d.y1 - d.y0; })
        .attr("fill", function(d) { return scaleColor(d.value); })
        .on("mouseover",function(d){
        	d3.select(this).style('fill', 'brown')
            tooltip
            .html(d.data.value + "<br/>"  + format(d.data.count))	
                .style("left", (d3.event.pageX) + "px")		
                .style("top", (d3.event.pageY - 28) + "px")
                .style("display", "inline-block")
        })
        .on("mouseout",function(d){
        	d3.select(this).style('fill', '')
           	tooltip.style("display", "none")
        })

    cell.append("clipPath")
        .attr("id", function(d) { return "clip-" + d.id; })
      .append("use")
        .attr("xlink:href", function(d) { return "#" + d.id; });

    cell.append("text")
        .attr("clip-path", function(d) { return "url(#clip-" + d.id + ")"; })
      .selectAll("tspan")
        .data(function(d) {  

        	var id = d.id.split(/(?=[A-Z][^A-Z])/g)
        	return  id; 
      	})
      .enter().append("tspan")
        .attr("x", 4)
        .attr("y", function(d, i) { return 13 + i * 10; })
        .text(function(d) {  return d; });

    /*cell.append("title")
        .text(function(d) {  return d.id + "\n" + format(d.data.count); }); */
}

function boxNumber2(params){
       var format = d3.format(",d");       
       this.append('span')             
               .transition()
        .duration(duration)
        .on("start", function repeat() {
          d3.active(this)
            .tween("text", function() {
                var that = d3.select(this),
                    i = d3.interpolateNumber(that.text().replace(/,/g, ""), params.docs.length);
                return function(t) { that.text(format(i(t)) + ' ' + params.title); };
            })
            .transition()
            .delay(delay)
        });
}


