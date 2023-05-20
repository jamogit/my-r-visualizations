// !preview r2d3 data=c(2.3, 2.6, 0.8, 1.95, 0.40, 0.20)

var barHeight = Math.floor(height / data.length);



d3.selectAll('rect')
  .data(data)
  .enter().append('rect')
    .attr('width', function(d) { return d * width; })
    .attr('height', barHeight)
    .attr('y', function(d, i) { return i * barHeight; })
    .attr('fill', 'steelblue');