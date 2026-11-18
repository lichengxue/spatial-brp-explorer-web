'use strict';
const $ = id => document.getElementById(id);
// The same interface can use the local R server or the browser's R runtime.
const modelRequest = (path, options) => window.brpBrowser
  ? window.brpBrowser.request(path, options) : fetch(path, options);
const RULES = [
  ['SpE-RecW', 'Recruitment weights', 'Global · movement retained', '#365fa0'],
  ['SpE-EqW', 'Equal population weights', '', '#b36b13'],
  ['SpE-SurvW', 'Survey biomass weights', '', '#8a4f80'],
  ['SpE-CatchW', 'Catch biomass weights', '', '#1c7f91'],
  ['SpD-RecW', 'Recruitment weights · omit movement', '', '#365fa0'],
  ['Natal-RecW', 'Count only natal-region spawning', '', '#56677c'],
  ['Pop-NoMove', 'Regional = population-specific · no movement', 'Regional / population-specific', '#8a5837'],
  ['Pop-Move', 'Joint population SPR targets', '', '#6a792e'],
  ['Reg-Move', 'Joint regional SPR targets', 'Region-specific', '#ba4652'],
];
const NO_MOVE_RULES=[
  ['SpD-EqW','Equal population weights · no movement','','#b36b13'],
  ['SpD-SurvW','Survey biomass weights · no movement','','#8a4f80'],
  ['SpD-CatchW','Catch biomass weights · no movement','','#1c7f91'],
];
const COLORS = Object.fromEntries([...RULES,...NO_MOVE_RULES].map(r => [r[0], r[3]]));
COLORS['Yield-SPR']='#172b49'; COLORS['Yield-Max']='#7b8491';
Object.assign(COLORS,{'MSY-Pattern':'#0086a8','MSY-System':'#7e2f48','MSY-Balanced':'#9b741c'});
// One line-style definition serves charts, legends, checkboxes and result rows.
const RULE_DASHES=Object.freeze({'Natal-RecW':'5 4','Pop-Move':'7 3','SpD-RecW':'3 3','SpD-EqW':'3 3','SpD-SurvW':'3 3','SpD-CatchW':'3 3','Yield-SPR':'10 3','Yield-Max':'2 4','MSY-Pattern':'8 3 2 3','MSY-System':'12 3 2 3','MSY-Balanced':'4 3'});
function ruleSwatch(rule) {
  const dash=RULE_DASHES[rule];
  return `<svg class="line-swatch" viewBox="0 0 36 10" width="36" height="10" aria-hidden="true"><line x1="2" x2="34" y1="5" y2="5" stroke="${COLORS[rule]||'#888'}" stroke-width="2" stroke-linecap="round" ${dash?`stroke-dasharray="${dash}"`:''}/></svg>`;
}
function ruleLabel(rule,recruitment=result?.settings.recruitment) {
  return rule==='Yield-Max'&&recruitment==='sr'?'Yield-Max · system MSY':rule;
}
const allResults=()=>result?[...result.brps,...(result.benchmarks||[]),...(result.msy||[])]:[];
let defaults, result = null, runId = null, resultRunId = null, busy = false, focused = null, changedDuringRun = false;
let biologyDefaults=null, customBiology={short:null,long:null}, biologyDraft=null, editingProfile='short', biologyDraftIsDefault=true;
let customSeasonalMovement=null,movementDraft=null,lastMovementMode='high',movementEditorPreviousMode='high';
const BIO_FIELDS=['selectivity1','selectivity2','weight1','weight2','maturity1','maturity2'];
let audienceScenarios=[],activeAudienceScenario=null,queuedAudienceScenario=null;
const esc = value => String(value ?? '').replace(/[&<>"']/g, c => ({'&':'&amp;','<':'&lt;','>':'&gt;','"':'&quot;',"'":'&#39;'}[c]));
const fmt = (value, digits=3) => value == null ? '—' : Number(value).toLocaleString('en-US', {maximumFractionDigits:digits, minimumFractionDigits:digits});
const rounded = value => Number(value).toLocaleString('en-US', {maximumFractionDigits:0});

function buildRules() {
  const groups=[['Global BRPs with movement',[...RULES.slice(0,4),RULES[5]]],['Global BRPs without movement',[RULES[4],...NO_MOVE_RULES]],['Population-specific BRP with movement',[RULES[7]]],['Regional BRP with movement',[RULES[8]]],['Regional BRP without movement',[RULES[6]]]];
  $('ruleList').innerHTML=groups.map(([title,rows])=>`<details class="rule-group"><summary><span>${title}</span><span class="rule-group-count"></span></summary><div class="rule-group-options">${rows.map(([name,description])=>`<label class="rule-row"><input type="checkbox" name="rule" value="${name}" ${RULES.some(r=>r[0]===name)?'checked':''}><span class="rule-copy"><strong>${name}</strong><small>${description}</small></span>${ruleSwatch(name)}</label>`).join('')}</div></details>`).join('');
  $('benchmarkSPRSwatch').innerHTML=ruleSwatch('Yield-SPR');$('benchmarkMaxSwatch').innerHTML=ruleSwatch('Yield-Max');
  for(const [id,rule] of [['msyPatternSwatch','MSY-Pattern'],['msyBalancedSwatch','MSY-Balanced']])$(id).innerHTML=ruleSwatch(rule);
  document.querySelectorAll('input[name=rule]').forEach(input => input.addEventListener('change', markDirty));
  $('surveyControls').innerHTML=`<div class="survey-head"><span>Season midpoint</span><span>R1</span><span>R2</span></div>`+[1,2,3,4].map(s=>`<div class="survey-row"><span>Season ${s} <small>${(s-.5)/4} yr</small></span>${[1,2].map(r=>`<label><input type="checkbox" name="survey" value="${r}-${s}" aria-label="Region ${r} season ${s} survey" checked></label>`).join('')}</div>`).join('');
}
function selectedRules() { return [...document.querySelectorAll('input[name=rule]:checked')].map(x=>x.value); }
function selectedBenchmarks() { return [...document.querySelectorAll('input[name=benchmark]:checked')].map(x=>x.value); }
function selectedMSY() { return [...document.querySelectorAll('input[name=msy]:checked:not(:disabled)')].map(x=>x.value); }
function updateCount() {
  const n = selectedRules().length;
  $('selectedCount').textContent = n;
  document.querySelectorAll('#ruleList .rule-group').forEach(group=>{
    const total=group.querySelectorAll('input[name=rule]').length, selected=group.querySelectorAll('input[name=rule]:checked').length;
    group.querySelector('.rule-group-count').textContent=`${selected}/${total} selected`;
  });
  $('toggleAll').textContent = n === document.querySelectorAll('input[name=rule]').length ? 'Clear BRPs' : 'Select all BRPs';
  const anySelected=n||selectedBenchmarks().length||selectedMSY().length;
  $('runButton').disabled = busy || !anySelected;
  $('loadAudienceScenario').disabled=busy||!audienceScenarios.length;
  $('copyAudienceLink').disabled=!activeAudienceScenario;
  $('runHint').textContent = !anySelected ? 'Select a BRP, yield benchmark or MSY comparison' : busy ? 'Model running · you can inspect the settings' : 'Base R calculations · stays on this computer';
}
function sideTab(settings) {
  $('brpControls').hidden = settings; $('modelControls').hidden = !settings;
  for(const [id, active] of [['brpTab',!settings], ['settingsTab',settings]]) {
    $(id).classList.toggle('active',active); $(id).setAttribute('aria-selected',String(active));
  }
  document.querySelector('.side-scroll').scrollTop = 0;
}
function getSettings() {
  const cfg = {rules:selectedRules(), structure:document.querySelector('input[name=population]:checked').value === 'NH' ? 'NH' : $('biology').value};
  for(const id of ['movement','recruitment','life_history','bh_calibration','projection','regional_weighting']) cfg[id] = $(id).value;
  for(const id of ['fishing_share1','historical_F','historical_years','R01','R02','target','years','sigmaR','seed','alpha','steepness','regional_share1']) cfg[id] = Number($(id).value);
  if(cfg.regional_weighting!=='custom')cfg.regional_share1=50;
  cfg.stochastic = $('stochastic').checked;
  cfg.survey_ensemble=$('survey_ensemble').checked;
  cfg.surveys=[...document.querySelectorAll('input[name=survey]:checked')].map(x=>x.value);
  cfg.benchmarks=selectedBenchmarks();
  cfg.msy=selectedMSY();
  cfg.biology=customBiology[$('life_history').value];
  cfg.seasonal_movement=cfg.movement==='custom'?customSeasonalMovement:null;
  return cfg;
}
function applySettings(cfg) {
  cfg={regional_weighting:'recruitment',regional_share1:50,...cfg};
  customSeasonalMovement=cfg.seasonal_movement?structuredClone(cfg.seasonal_movement):null;
  lastMovementMode=cfg.movement;
  document.querySelector(`input[name=population][value=${cfg.structure === 'NH' ? 'NH' : 'MP'}]`).checked = true;
  $('biology').value = cfg.structure === 'NH' ? 'MPO' : cfg.structure;
  for(const key in cfg) if($(key) && key !== 'structure' && key !== 'biology') {
    if($(key).type === 'checkbox') $(key).checked = cfg[key]; else $(key).value = cfg[key];
  }
  document.querySelectorAll('input[name=rule]').forEach(x=>x.checked=cfg.rules.includes(x.value));
  const benchmarks=new Set(cfg.benchmarks===true?['Yield-SPR','Yield-Max']:(cfg.benchmarks||[]));
  if((cfg.msy||[]).includes('MSY-System'))benchmarks.add('Yield-Max');
  document.querySelectorAll('input[name=benchmark]').forEach(x=>x.checked=benchmarks.has(x.value));
  document.querySelectorAll('input[name=msy]').forEach(x=>x.checked=(cfg.msy||[]).includes(x.value));
  document.querySelectorAll('input[name=survey]').forEach(x=>x.checked=cfg.surveys.includes(x.value));
  customBiology[cfg.life_history]=cfg.biology;
  const s=String(cfg.fishing_share1); $('fishingPreset').value = ['70','50','30'].includes(s)?s:'custom';
  updateControls(); updateCount();
}
function updateControls() {
  const nh = document.querySelector('input[name=population]:checked').value === 'NH';
  $('biologyWrap').hidden = nh; $('bhControls').hidden = $('recruitment').value !== 'sr';
  const regionalMode=$('regional_weighting').value;
  $('regionalCustomWeights').hidden=regionalMode!=='custom';
  $('regional_share1').disabled=regionalMode!=='custom';
  const originShare=Number($('regional_share1').value);
  $('regionalShareLabel').textContent=`Origin 1: ${originShare}% · Origin 2: ${Number((100-originShare).toFixed(6))}%`;
  $('regionalWeightDescription').textContent={recruitment:'Use biological reference recruitment: mean / R₀ for fixed-mean recruitment, or rule-specific equilibrium recruitment for Beverton–Holt.',equal:'Give one recruit to each birth origin, regardless of actual recruitment.',survey:'Freeze selected historical survey shares and assign each region’s share to its corresponding birth origin.',catch:'Freeze final historical regional catch shares and assign each share to its corresponding birth origin.',custom:'Set a fixed mixture of the two birth origins. This does not replace the biological recruitment inputs.'}[regionalMode];
  $('regionalWeightHoming').textContent=nh?'Complete natal homing: positive origin weights cancel within each spawning region, so these choices give the same regional SPR.':'Metapopulation: origin weights can change regional SPR and the jointly solved regional fishing rates.';
  const bh=$('recruitment').value==='sr';
  document.querySelectorAll('input[name=msy]').forEach(x=>{x.disabled=!bh;if(!bh)x.checked=false;});
  $('benchmarkMaxLabel').textContent=ruleLabel('Yield-Max',bh?'sr':'mean');
  $('benchmarkMaxDescription').textContent=bh?'Maximize total sustainable yield; freely optimize F₁ and F₂':'Highest yield with recruitment held at its mean';
  $('msyAvailability').textContent=bh?'Yield-Max above is system MSY. These additional options answer different questions: keep your fishing ratio, or compare the two component maxima.':'These additional comparisons require Beverton–Holt recruitment. With Beverton–Holt, Yield-Max above is already system MSY. Switching changes recruitment for all selected BRPs and projections.';
  $('enableMSY').hidden=bh;
  $('noiseControls').hidden = !$('stochastic').checked;
  $('alphaWrap').hidden = $('bh_calibration').value !== 'slope';
  $('steepnessWrap').hidden = $('bh_calibration').value !== 'steepness';
  $('surveyAggregationNote').textContent=$('survey_ensemble').checked?'Average the selected survey indices in each region, then normalize the two regional averages. All surveys use the same catchability and selectivity.':'Sum the selected indices within each region, then normalize the two sums. Selecting more surveys in one region increases that region’s weight.';
  $('biologySummary').textContent=customBiology[$('life_history').value]?'Custom selectivity, weight and maturity are active for this life-history profile.':'Using model biology and fleet selectivity.';
  const target=$('target').value;
  $('targetPreset').value=[...$('targetPreset').options].some(o=>o.value===target)?target:'custom';
  $('targetLabel').textContent=`F${target || 'x'}%`;
  const share=Number($('fishing_share1').value);
  $('share1Label').textContent=`${share}%`; $('share2Label').textContent=`${100-share}%`;
  $('allocation1').style.width=`${share}%`;
  const schedule=movementRates(getSettings());
  $('movementDescription').textContent=$('movement').value==='custom'?schedule.map((m,k)=>nh&&k===1?'S2: complete natal return':`S${k+1}: ${m.m12}% R1→2 / ${m.m21}% R2→1`).join(' · '):`At each ordinary transfer: ${schedule[0].m12}% move from Region 1 to 2; ${schedule[0].m21}% move from 2 to 1. ${nh?'Season 2 is replaced by natal return.':'Ordinary movement follows all four seasons.'}`;
}
function markDirty() {
  activeAudienceScenario=null;
  if(location.hash.startsWith('#scenario='))history.replaceState(null,'',location.pathname+location.search);
  updateControls(); updateCount();
  updateEquationReference();
  renderAudienceGuide();
  if(result) $('stale').hidden = false;
  if(busy) changedDuringRun = true;
}
function describe(cfg) {
  const structure={NH:'Natal homing',MPO:'Metapopulation · origin biology',MPD:'Metapopulation · destination biology'}[cfg.structure];
  return `${structure} · ${{high:'High directional',low:'Low symmetric',none:'No',custom:'Custom seasonal'}[cfg.movement]} movement · ${cfg.recruitment==='mean'?'Mean recruitment':'Beverton–Holt'}${cfg.stochastic?' + variability':''}`;
}
function regionalWeightLabel(cfg) {
  return {recruitment:'Reference recruitment',equal:'Equal origins · 50 : 50',survey:'Historical survey-share proxy',catch:'Historical catch-share proxy',custom:`Custom origins · ${cfg.regional_share1} : ${Number((100-cfg.regional_share1).toFixed(6))}`}[cfg.regional_weighting||'recruitment'];
}
function previewAudienceScenario() {
  const scenario=audienceScenarios.find(s=>s.id===$('audienceScenario').value);
  $('audienceScenarioDescription').textContent=scenario?.description||'';
}
function loadAudienceScenario(id,run=true) {
  if(busy) {queuedAudienceScenario=id;return;}
  const scenario=audienceScenarios.find(s=>s.id===id);if(!scenario)return;
  customBiology={short:null,long:null};
  applySettings(structuredClone(scenario.settings));
  activeAudienceScenario=scenario;$('audienceScenario').value=id;
  $('audienceShareMessage').textContent='';
  $('populationMetric').value='ssb';focused=null;
  if(result)$('stale').hidden=false;
  else {
    $('rulesMetric').innerHTML=`— <span>/ ${scenario.settings.rules.length}</span>`;
    $('historyStatus').textContent=`Historical conditioning: ${scenario.settings.historical_years} years before projection.`;
    updateResultHeading();
  }
  previewAudienceScenario();renderAudienceGuide();updateEquationReference();updateCount();
  if(run) {
    history.replaceState(null,'',`#scenario=${encodeURIComponent(id)}`);
    sideTab(false);selectResultView('dynamicsView');calculate();
  }
}
function renderAudienceGuide() {
  document.body.classList.toggle('audience-mode',!!activeAudienceScenario);
  $('audienceGuideTitle').textContent=activeAudienceScenario?.title||'Custom exploration';
  $('audienceGuideFocus').textContent=activeAudienceScenario?.focus||'Adjust the controls to explore your own comparison. The Equations tab explains each target; the SPR table reports regional and recruitment-weighted global results.';
  const reg=result?.brps.find(r=>r.rule==='Reg-Move'&&r.status!=='unavailable');
  const global=result?.brps.find(r=>r.rule==='SpE-RecW'&&r.status!=='unavailable');
  const percent=v=>v==null?'—':`${fmt(100*v,1)}%`;
  const insights=[];
  if(reg)insights.push(`Reg-Move: regional SPR ${percent(reg.regional_spr1)} / ${percent(reg.regional_spr2)}; global RecW ${percent(reg.global_spr_rec)}.`);
  if(global)insights.push(`SpE-RecW: regional SPR ${percent(global.regional_spr1)} / ${percent(global.regional_spr2)}.`);
  $('audienceGuideResult').textContent=busy?'Calculating the selected setup…':result&&insights.length?'Latest calculated result · '+insights.join(' '):'';
}
async function copyAudienceLink() {
  if(!activeAudienceScenario)return;
  const url=new URL(location.href);url.hash=`scenario=${encodeURIComponent(activeAudienceScenario.id)}`;
  try {
    await navigator.clipboard.writeText(url.href);
    $('audienceShareMessage').textContent=['localhost','127.0.0.1','[::1]'].includes(location.hostname)?'Copied. This local link works on this computer; share the app ZIP for other computers.':'Scenario link copied.';
  } catch(error) {$('audienceShareMessage').textContent=`Scenario link: ${url.href}`;}
}
function setStatus(message, state='running') {
  $('status').textContent=message; $('statusDot').className=`status-dot ${state}`;
}
async function calculate() {
  if(busy || (!selectedRules().length&&!selectedBenchmarks().length&&!selectedMSY().length)) return;
  const selectedSurveys=[...document.querySelectorAll('input[name=survey]:checked')].map(x=>x.value);
  if([1,2].some(r=>!selectedSurveys.some(s=>s.startsWith(`${r}-`)))) {
    $('error').textContent='Select at least one survey in each region to calculate regional weights.';
    $('error').hidden=false; sideTab(true); return;
  }
  // Native validation reports the exact field that needs attention.
  if(!$('target').reportValidity()) return;
  if($('regional_weighting').value==='custom'&&!$('regional_share1').reportValidity())return;
  if(!$('modelControls').reportValidity()) { sideTab(true); $('modelControls').reportValidity(); return; }
  const cfg=getSettings(); busy=true; changedDuringRun=false; $('error').hidden=true; updateCount();
  renderAudienceGuide();
  $('runButton').innerHTML='Calculating…'; setStatus('Starting the R operating model…');
  try {
    const response=await modelRequest('/api/runs',{method:'POST',headers:{'Content-Type':'application/json'},body:JSON.stringify(cfg)});
    const job=await response.json(); if(!response.ok) throw new Error(job.error || 'Unable to start calculation.');
    runId=job.id;
    while(true) {
      await new Promise(resolve=>setTimeout(resolve,850));
      const poll=await modelRequest(`/api/runs/${runId}`); const current=await poll.json();
      if(!poll.ok || current.state==='error') throw new Error(current.error || 'Model run failed.');
      setStatus(current.progress || 'Calculating reference points…');
      if(current.state==='complete') {
        result=current.result; focused=null;
        $('download').disabled=false; $('stale').hidden=!changedDuringRun;
        renderResults();
        const valid=result.brps.filter(x=>x.status!=='unavailable').length;
        const benchmarkCount=[...result.benchmarks,...(result.msy||[])].filter(x=>x.status!=='unavailable').length;
        setStatus(`Calculated ${valid} BRP${valid===1?'':'s'} and ${benchmarkCount} yield benchmark${benchmarkCount===1?'':'s'} in ${result.seconds}s · ${cfg.years} projection years`, 'done');
        break;
      }
    }
  } catch(error) {
    $('error').textContent=error.message; $('error').hidden=false;
    setStatus('Calculation could not finish. Review the message above.', '');
  } finally {
    busy=false; $('runButton').innerHTML='<span aria-hidden="true">▶</span> Calculate & project'; updateCount();renderAudienceGuide();
    if(queuedAudienceScenario) {const next=queuedAudienceScenario;queuedAudienceScenario=null;loadAudienceScenario(next);}
  }
}

function renderResults() {
  resultRunId=runId;
  const cfg=result.settings;
  updateResultHeading();
  $('targetMetric').innerHTML=`${cfg.target}<span>%</span>`;
  $('patternMetric').innerHTML=`${cfg.fishing_share1} <span>:</span> ${100-cfg.fishing_share1}`;
  const valid=result.brps.filter(x=>x.status!=='unavailable').length;
  $('rulesMetric').innerHTML=`${valid} <span>/ ${result.brps.length}</span>`;
  $('rulesNote').textContent=valid===result.brps.length?'All selected formulations available':`${result.brps.length-valid} without an accepted solution`;
  const benchmarkCount=[...result.benchmarks,...(result.msy||[])].filter(x=>x.status!=='unavailable').length;
  if(!result.brps.length) {$('rulesMetric').innerHTML=`${benchmarkCount}<span> benchmark${benchmarkCount===1?'':'s'}</span>`;$('rulesNote').textContent='Yield benchmarks selected independently';}
  renderLegend(); renderCharts(); renderTable(); renderDetails(); renderMSY(); renderAudienceGuide();
  $('plotCaption').textContent=`Projection year 1 follows ${cfg.historical_years} historical years. ${cfg.projection==='fixed'?'Each BRP’s fishing mortality is held constant.':'Each SPR BRP uses its own annual biomass feedback.'} ${cfg.stochastic?`One paired stochastic realization (σR = ${cfg.sigmaR}, seed ${cfg.seed}); curves are not uncertainty intervals.`:'Deterministic recruitment; no random variation.'} ${cfg.benchmarks.length||(cfg.msy||[]).length?'Yield and MSY benchmark curves always hold optimized F constant. ':''}SSB and catch use kg in this synthetic model. Click a legend entry to highlight a rule; click again to show all.`;
  const history=result.history?.[0];
  if(history) {
    const stable=history.stable==='TRUE';
    $('historyStatus').classList.toggle('history-warning',!stable);
    const gap=value=>value==null?'unavailable':`${fmt(100*value,2)}%`;
    $('historyStatus').textContent=`${cfg.historical_years}-year historical period · ${stable?'Within 1% of equilibrium':'Still settling: consider a longer historical period'} · SSB gap ${gap(history.max_ssb_gap)}; abundance-at-age gap ${gap(history.max_numbers_gap)}.`;
  }
}
function renderLegend() {
  if(!result) return;
  $('legend').innerHTML=allResults().filter(r=>r.status!=='unavailable').map(r=>`<button data-rule="${r.rule}" aria-pressed="${focused===r.rule}" class="${focused?(focused===r.rule?'focused':'dimmed'):''}">${ruleSwatch(r.rule)}${esc(ruleLabel(r.rule))}${r.status==='equivalent'?' ≡':''}</button>`).join('');
  $('legend').querySelectorAll('button').forEach(b=>b.addEventListener('click',()=>{focused=focused===b.dataset.rule?null:b.dataset.rule; renderLegend();renderCharts();}));
}
const METRICS={ssb:['Spawning biomass (kg)','Region'],depletion:['SSB / unfished SSB','Region'],stock_ssb:['Spawning biomass (kg)','Natal population'],abundance:['Fish at year start','Region'],recruitment:['Recruits at year start','Recruitment unit'],F:['Fishing mortality (yr⁻¹)','Region']};
function renderCharts() {
  const metric=$('populationMetric').value;
  for(const region of [1,2]) {
    $(`popTitle${region}`).textContent=`${METRICS[metric][1]} ${region}`;
    $(`popUnit${region}`).textContent=METRICS[metric][0];
    chart($(`population${region}`), `${metric}${region}`, metric==='depletion', metric==='depletion'?'Regional SSB / unfished SSB':METRICS[metric][0]);
    chart($(`catch${region}`),`catch${region}`,false,'Annual catch biomass (kg)');
    chart($(`fishing${region}`),`F${region}`,false,'Applied fully selected fishing mortality (yr⁻¹)');
  }
}
function compact(value) {
  if(Math.abs(value)>=1e6)return `${(value/1e6).toFixed(1)}m`;
  if(Math.abs(value)>=1000)return `${(value/1000).toFixed(value>=1e4?0:1)}k`;
  if(value===0)return '0';
  return value.toLocaleString('en-US',{maximumFractionDigits:Math.abs(value)<2?2:1});
}
function chart(container, field, ratio, label) {
  if(!result || !result.trajectories.length) {
    container.innerHTML='<div class="chart-empty"><span class="empty-line"></span><span>Calculate a scenario to see the dynamics</span></div>'; return;
  }
  const width=Math.max(280, container.clientWidth), height=Math.max(205,Math.min(278,width*.5));
  const pad={l:46,r:13,t:15,b:33}, w=width-pad.l-pad.r, h=height-pad.t-pad.b;
  const series=allResults().filter(r=>r.status!=='unavailable').map(r=>({rule:r.rule,rows:result.trajectories.filter(t=>t.rule===r.rule)}));
  const values=series.flatMap(s=>s.rows.map(t=>t[field])).filter(v=>Number.isFinite(v));
  let max=Math.max(...values,1e-6)*1.08; if(ratio) max=Math.max(max,1.05);
  const x=year=>pad.l+(year-1)/Math.max(1,result.settings.years-1)*w;
  const y=value=>pad.t+h-value/max*h;
  let svg=`<svg viewBox="0 0 ${width} ${height}" role="img" aria-label="${esc(label)}, ${result.settings.years} projection years, ${series.length} reference point curves"><title>${esc(label)}</title>`;
  for(let i=0;i<=4;i++) {
    const val=max*i/4, yy=y(val);
    svg+=`<line x1="${pad.l}" x2="${width-pad.r}" y1="${yy}" y2="${yy}" stroke="#dfe5ef" ${i?'stroke-dasharray="3 4"':''}/><text x="${pad.l-9}" y="${yy+3}" text-anchor="end" fill="#74839a" font-size="9">${compact(val)}</text>`;
  }
  const ticks=[1,...[.25,.5,.75,1].map(f=>Math.round(result.settings.years*f))];
  for(const year of [...new Set(ticks)]) svg+=`<text x="${x(year)}" y="${height-17}" text-anchor="middle" fill="#74839a" font-size="9">${year}</text>`;
  svg+=`<text x="${pad.l+w/2}" y="${height-2}" text-anchor="middle" fill="#74839a" font-size="9">Projection year</text>`;
  // SSB depletion is a biomass measure, so it does not inherit the SPR target line.
  if(ratio) svg+=`<line x1="${pad.l}" x2="${width-pad.r}" y1="${y(1)}" y2="${y(1)}" stroke="#a5b5cc" stroke-dasharray="5 5"/><text x="${width-pad.r-3}" y="${y(1)-5}" text-anchor="end" fill="#7689a5" font-size="8">Unfished SSB</text>`;
  const ordered=[...series].sort((a,b)=>Number(a.rule===focused)-Number(b.rule===focused));
  ordered.forEach((s,i)=>{
    const points=s.rows.map(t=>`${x(t.year).toFixed(2)},${y(t[field]).toFixed(2)}`).join(' ');
    const dim=focused && s.rule!==focused;
    const dash=RULE_DASHES[s.rule];
    svg+=`<polyline data-rule="${s.rule}" points="${points}" fill="none" stroke="${COLORS[s.rule]}" stroke-width="${s.rule===focused?2.8:1.7}" opacity="${dim?.12:.86}" stroke-linecap="round" stroke-linejoin="round" ${dash?`stroke-dasharray="${dash}"`:''}/>`;
  });
  svg+=`<line class="hover-line" x1="0" x2="0" y1="${pad.t}" y2="${pad.t+h}" stroke="#667fa2" stroke-dasharray="2 3" visibility="hidden"/><rect class="hover-target" x="${pad.l}" y="${pad.t}" width="${w}" height="${h}" fill="transparent"/></svg>`;
  container.innerHTML=svg;
  const svgEl=container.querySelector('svg'), line=container.querySelector('.hover-line');
  container.querySelector('.hover-target').addEventListener('pointermove',event=>{
    const bounds=svgEl.getBoundingClientRect(), local=(event.clientX-bounds.left)/bounds.width*width;
    const year=Math.max(1,Math.min(result.settings.years,Math.round((local-pad.l)/w*(result.settings.years-1)+1)));
    line.setAttribute('x1',x(year));line.setAttribute('x2',x(year));line.setAttribute('visibility','visible');
    const entries=focused?series.filter(s=>s.rule===focused):series;
    $('chartTooltip').innerHTML=`<strong>Year ${year} · ${esc(label)}</strong>`+entries.map(s=>`<div class="tooltip-row"><span style="--c:${COLORS[s.rule]}">${esc(ruleLabel(s.rule))}</span><b>${fmt(s.rows[year-1][field],ratio||field.startsWith('F')?3:1)}</b></div>`).join('');
    $('chartTooltip').hidden=false;
    const tw=$('chartTooltip').offsetWidth, th=$('chartTooltip').offsetHeight;
    $('chartTooltip').style.left=`${Math.min(event.clientX+16,window.innerWidth-tw-12)}px`;
    $('chartTooltip').style.top=`${Math.max(8,Math.min(event.clientY+12,window.innerHeight-th-12))}px`;
  });
  container.onpointerleave=()=>{line.setAttribute('visibility','hidden');$('chartTooltip').hidden=true;};
}
function renderTable() {
  const cfg=result.settings;
  $('regionalResultNote').textContent=`Regional SPR uses ${regionalWeightLabel(cfg)}. The same origin mix is used in numerator and denominator; its resolved values are in Model details and the export. Global SPR · RecW always uses biological reference recruitment. ${cfg.regional_weighting==='recruitment'?'Both regional targets imply that matching global RecW target.':'Alternative regional weights can give a different global RecW value in a metapopulation.'}`;
  const summary=result.benchmark_summary?.[0];
  const msyBase=result.msy_summary?.find(x=>x.rule==='MSY-System')||result.msy_summary?.find(x=>x.rule==='MSY-Pattern');
  const yieldBase=summary?.Opt_yield??summary?.YieldMax_yield??msyBase?.yield;
  const denominator=summary?.Opt_yield!=null?'Yield-SPR':summary?.YieldMax_yield!=null?'Yield-Max':msyBase?.rule;
  $('yieldRatioHeader').textContent=denominator?`Yield / ${ruleLabel(denominator)}`:'Yield / selected benchmark';
  $('yieldReferenceNote').textContent=denominator?`Yield ratios use ${ruleLabel(denominator)} as the denominator.`:'Select a yield benchmark to display yield ratios.';
  $('comparisonTable').querySelector('tbody').innerHTML=allResults().map(r=>{
    const fields=['F1','F2','F_global','global_spr_rec','pop_spr1','pop_spr2','regional_spr1','regional_spr2'];
    return `<tr class="${r.status==='unavailable'?'unavailable':''}"><td>${ruleSwatch(r.rule)} ${esc(ruleLabel(r.rule))}</td>`+fields.map(f=>`<td class="${f.includes('spr')&&r[f]!=null&&r[f]<result.settings.target/100-1e-6?'below-target':''}">${fmt(r[f])}</td>`).join('')+`<td>${r.eq_catch1==null?'—':fmt(r.eq_catch1+r.eq_catch2,1)}</td><td>${yieldBase&&r.eq_catch1!=null?fmt(100*(r.eq_catch1+r.eq_catch2)/yieldBase,1)+'%':'—'}</td><td><span class="status-label ${r.status==='unavailable'?'warning':''}">${{calculated:'Calculated',equivalent:'Equivalent',unavailable:'Unavailable',benchmark:'Benchmark'}[r.status]}</span></td></tr>`;
  }).join('');
  $('solutionNotes').innerHTML=allResults().filter(r=>r.note).map(r=>`<p class="solution-note"><strong>${esc(ruleLabel(r.rule))}</strong>${esc(r.note)}</p>`).join('')+(result.settings.benchmarks.length?`<p class="solution-note">Yield benchmarks search independently over 0 ≤ F₁, F₂ ≤ 10 yr⁻¹ using the feasible fishing grid, multiple optimizer starts, and explicit one-region closures. These are bounded numerical searches. The constrained benchmark holds one regional origin mix fixed (${esc(summary?.reference_basis || 'unfished R0 composition')}). With mean recruitment, these are fixed-recruitment equilibrium yield comparisons, not MSY.</p>`:'');
}
function renderMSY() {
  const rows=result.msy_summary||[],joint=result.msy_joint?.[0];
  const benchmarks=(result.benchmarks||[]).filter(r=>r.status!=='unavailable');
  const benchmarkHTML=benchmarks.length?`<p>${result.settings.recruitment==='sr'?'With Beverton–Holt recruitment, Yield-Max is the system-MSY estimate: F₁ and F₂ jointly maximize total sustainable equilibrium yield.':'With fixed-mean recruitment, Yield-Max maximizes equilibrium yield with recruitment held fixed; it is not an MSY estimate.'} Yield-SPR adds the two regional SPR constraints. Both comparisons let the fishing ratio vary.</p><div class="table-wrap"><table><thead><tr><th>Yield comparison</th><th>F₁</th><th>F₂</th><th>Total yield (kg/yr)</th><th>Regional SSB 1</th><th>Regional SSB 2</th></tr></thead><tbody>${benchmarks.map(r=>`<tr><th>${ruleSwatch(r.rule)} ${esc(ruleLabel(r.rule))}</th><td>${fmt(r.F1,4)}</td><td>${fmt(r.F2,4)}</td><td>${fmt(r.eq_catch1+r.eq_catch2,2)}</td><td>${fmt(r.eq_ssb1,2)}</td><td>${fmt(r.eq_ssb2,2)}</td></tr>`).join('')}</tbody></table></div>`:'';
  if(!rows.length) {
    const failure=result.msy?.find(r=>r.status==='unavailable')?.note||result.benchmarks?.find(r=>r.status==='unavailable')?.note;
    $('msyResults').innerHTML=benchmarkHTML+(failure?`<p>${esc(failure)}</p>`:benchmarks.length?'<p>Expand “Fishing pattern & component maxima” in the yield controls for the optional fixed-pattern MSY and simultaneous-maxima comparisons.</p>':'<p>Select Yield-SPR or Yield-Max under Reference points. The additional fixed-pattern and component-maxima comparisons require Beverton–Holt recruitment. SPR remains the default.</p>');
    return;
  }
  const basis=result.settings.structure==='NH'?'Natal population catch':'Regional catch';
  $('msyResults').innerHTML=benchmarkHTML+`<h4>Additional MSY comparisons</h4><p>MSY maximizes deterministic equilibrium yield: <code>Y(F₁,F₂) = Σₛ Rₛ(F₁,F₂) × YPRₛ(F₁,F₂)</code>. Recruitment solves the Beverton–Holt equilibrium at every trial F. These searches retain the selected movement and biology, and apply no SPR constraints. They do not optimize stochastic or biomass-feedback performance.</p><p><strong>Joint system optimization</strong> chooses F₁ and F₂ together to maximize total yield. It can reduce one component’s biomass or yield. <strong>Simultaneous component maxima</strong> asks whether one fishing pair achieves both separate maxima; that is a different objective.</p><div class="table-wrap"><table><thead><tr><th>Comparison</th><th>F₁</th><th>F₂</th><th>Total yield (kg/yr)</th><th>${basis} 1</th><th>${basis} 2</th><th>Regional SSB 1</th><th>Regional SSB 2</th><th>Recruitment 1</th><th>Recruitment 2</th></tr></thead><tbody>${rows.map(r=>`<tr><th>${ruleSwatch(r.rule)} ${esc(r.rule)}</th>${['F1','F2','yield','component1_yield','component2_yield','eq_ssb1','eq_ssb2','R1','R2'].map(k=>`<td>${fmt(r[k],k.startsWith('F')?4:2)}</td>`).join('')}</tr>`).join('')}</tbody></table></div><p class="small muted">MSY-Pattern keeps the selected F ratio; Yield-Max is the freely optimized system MSY when Beverton–Holt recruitment is used. MSY-Balanced maximizes the smaller fraction of the two separate component maxima, and is a compromise rather than system MSY. Curves always hold these F values constant, including when SPR rules use feedback. MSY recruitment-weighted diagnostics use fixed R₀; displayed regional diagnostics use the selected origin mix.</p>`;
  if(joint) {
    $('msyResults').innerHTML+=`<h4>Can both components attain their separate maxima?</h4><p><strong>${esc(joint.interpretation)}.</strong> At the balanced point, component 1 reaches ${fmt(100*joint.fraction1,2)}% and component 2 reaches ${fmt(100*joint.fraction2,2)}% of its separately optimized maximum. Each separate maximum is optimized over the same box, 0 ≤ F₁,F₂ ≤ 10. A match uses a relative tolerance of ${fmt(100*joint.tolerance,2)}%.</p><p>The sum of separate maxima is ${fmt(joint.sum_separate_maxima,2)} kg/yr; the best total system yield found is ${fmt(joint.system_MSY,2)} kg/yr at (${fmt(joint.system_F1,4)}, ${fmt(joint.system_F2,4)}). Adding separate maxima does not make their sum jointly attainable.</p><div class="table-wrap"><table><thead><tr><th>Objective optimized alone</th><th>F₁</th><th>F₂</th><th>Component 1 catch</th><th>Component 2 catch</th><th>Total catch</th></tr></thead><tbody>${(result.msy_components||[]).map(r=>`<tr><th>${basis} ${r.component}</th>${['F1','F2','component1_yield','component2_yield','total_yield'].map(k=>`<td>${fmt(r[k],k.startsWith('F')?4:2)}</td>`).join('')}</tr>`).join('')}</tbody></table></div>`;
  }
  $('msyResults').innerHTML+=`<h4>Numerical checks & limits</h4><p>Bounded searches use a grid, several nlminb starts and explicit searches of all four box edges. These are the highest values found, with no mathematical certificate of global optimality. A result on the upper F boundary is checked again with a bound of 20. Largest annual equilibrium-state residual: ${esc(Math.max(...rows.map(r=>r.equilibrium_residual)).toExponential(2))}. ${rows.some(r=>r.upper_sensitive==='TRUE')?'Warning: at least one result improves with the larger F bound.':''}</p><p>In metapopulations the two components here are catches by region, where reproduction occurs; natal-origin catch groups are not separate reproductive populations. Under natal homing, the components are catches by natal origin across both fishing regions.</p>`;
}
function renderDetails() {
  const cfg=result.settings, cond=result.conditioning;
  const tableRows=[['Mean / unfished recruitment R₀',...cond.map(r=>fmt(r.R0,0))],['Historical fully selected F',...cond.map(r=>fmt(r.historical_F))],['Last-5-year reference recruitment',...cond.map(r=>fmt(r.reference_recruitment,2))],['Historical survey weights',...cond.map(r=>fmt(r.survey_weight,4))],['Historical catch weights',...cond.map(r=>fmt(r.catch_weight,4))],['Unfished regional SSB (kg)',...cond.map(r=>fmt(r.unfished_ssb,1))],['Conditioned regional SSB (kg)',...cond.map(r=>fmt(r.initial_ssb,1))]];
  if(cfg.recruitment==='sr') tableRows.push(['Beverton–Holt slope α',...cond.map(r=>fmt(r.BH_alpha))],['Beverton–Holt β',...cond.map(r=>fmt(r.BH_beta,6))],['Implied steepness h',...cond.map(r=>fmt(r.BH_steepness,4))]);
  $('modelDetails').innerHTML=`<h4>Current result settings</h4><p>${esc(describe(cfg))}. ${cfg.life_history==='short'?'Short-lived: 10 ages, M = 0.35 yr⁻¹.':'Long-lived: 30 ages, M = 0.10 yr⁻¹.'} Last age is a plus group. One fleet fishes in each region.</p><h4>Seasonal schedule</h4><p>Four seasons of 0.25 years. Mortality precedes end-of-season movement. Spawning occurs at 0.625 years, before season-3 movement. ${cfg.structure==='NH'?'All surviving fish return to their natal region at the end of season 2.':'Fish spawn where they are; each region’s spawners produce that region’s recruits.'}</p><h4>Conditioning & weights</h4><p>All rules start from the same ${cfg.historical_years}-year historical conditioning. Survey and catch weights are frozen at the last historical year. Recruitment weights use each rule’s reference recruitment; Beverton–Holt reference recruitment is iterated to match the rule’s deterministic equilibrium.</p><table><thead><tr><th>Input / diagnostic</th><th>Region 1</th><th>Region 2</th></tr></thead><tbody>${tableRows.map(row=>`<tr>${row.map((c,i)=>`<${i?'td':'th'}>${c}</${i?'td':'th'}>`).join('')}</tr>`).join('')}</tbody></table><h4>Interpreting these results</h4><ul><li>SPR measures lifetime spawning per recruit relative to unfished. An SPR target of ${cfg.target}% does not imply biomass at ${cfg.target}% of unfished biomass.</li><li>Birth-population SPR groups fish by natal origin. Regional SPR groups spawners by where they spawn, using the selected birth-origin mix. Alternative mixes define spawning-output indices; biological reference recruitment still determines biomass targets.</li><li>Global rules preserve the reference ratio of fully selected fishing mortalities. This ratio does not describe catch allocation.</li><li>Pop-Move and Reg-Move jointly solve two SPR equations. A missing feasible solution is shown explicitly; the adapter does not substitute an approximate fit.</li><li>Optional lognormal recruitment variation is mean-corrected using exp(ε − σR²/2), with independent draws for the two recruitment units and paired draws across rules. It creates one realization, not a distribution of outcomes.</li><li>Fishing feedback, if enabled, is the model’s rule-specific biomass ramp with annual updates. Static BRPs remain deterministic.</li></ul><h4>Results export</h4><p>The export includes settings, reference points, trajectories, biology, weights and the R execution log.</p>`;
  const mixtureRows=allResults().filter(r=>r.status!=='unavailable'&&r.regional_weight1!=null);
  $('modelDetails').innerHTML+=`<h4>Regional spawning mixture</h4><p>${esc(regionalWeightLabel(cfg))}. The table gives the origin weights actually used for each result's regional SPR. Survey/catch shares are frozen historical proxies assigned by birth region. Recruitment-weighted regional diagnostics in the export (regional_spr_rec1/2) retain the biological reference mixture. Index weights are separate from biological recruitment. Changing Reg-Move fishing rates can change its equilibrium recruitment and biomass. Other BRP fishing rates and unconstrained yield/MSY objectives keep their own definitions.</p><table><thead><tr><th>Reference point</th><th>Origin 1 weight</th><th>Origin 2 weight</th></tr></thead><tbody>${mixtureRows.map(r=>`<tr><th>${esc(ruleLabel(r.rule))}</th><td>${fmt(r.regional_weight1,4)}</td><td>${fmt(r.regional_weight2,4)}</td></tr>`).join('')}</tbody></table>`;
  $('modelDetails').innerHTML+=`<h4>Eight seasonal surveys</h4><p>All eight indices sample the same final historical year at the midpoint of each season. The weighting source is ${cfg.survey_ensemble?'the average of selected surveys within each region':'the sum of selected surveys within each region'}, normalized across the two regions. Survey catchability is 0.1 and selectivity is the model’s commercial-fleet selectivity.</p><table><thead><tr><th>Region</th><th>Season</th><th>Time (yr)</th><th>Survey index</th><th>Used for weights</th></tr></thead><tbody>${result.surveys.map(s=>`<tr><td>${s.region}</td><td>${s.season}</td><td>${s.time}</td><td>${fmt(s.index,3)}</td><td>${s.selected==='TRUE'?'Yes':'No'}</td></tr>`).join('')}</tbody></table>`;
  const schedule=result.movement_schedule||movementRates(cfg).map((m,k)=>({season:k+1,transfer_time:(k+1)/4,natal_return:cfg.structure==='NH'&&k===1?'TRUE':'FALSE',applied_m12:m.m12/100,applied_m21:m.m21/100}));
  $('modelDetails').innerHTML+=`<h4>Movement schedule used</h4><p>${cfg.movement==='custom'?'Custom seasonal probabilities':'Model preset'}. Transfers occur at season end after mortality, and this schedule repeats each year. Under natal homing the second transfer returns each natal population home, rather than using an ordinary regional transfer matrix.</p><table><thead><tr><th>Season end</th><th>Time (yr)</th><th>R1 → R2</th><th>R2 → R1</th><th>Transfer</th></tr></thead><tbody>${schedule.map(s=>`<tr><td>${s.season}</td><td>${s.transfer_time}</td><td>${s.natal_return==='TRUE'?'—':fmt(100*s.applied_m12,2)+'%'}</td><td>${s.natal_return==='TRUE'?'—':fmt(100*s.applied_m21,2)+'%'}</td><td>${s.natal_return==='TRUE'?'Complete natal return':'Ordinary movement'}</td></tr>`).join('')}</tbody></table>`;
  const bioType=cfg.structure==='MPD'?'Region':'Natal population';
  $('modelDetails').innerHTML+=`<h4>Actual age-specific inputs</h4><p>${cfg.biology?'Custom inputs':'Model defaults'}. Fishing selectivity belongs to the fleet in each region. Weight and maturity belong to each ${cfg.structure==='MPD'?'region':'natal population'}. Survey selectivity is the standard model survey-gear curve, separate from fishing gear.</p><div class="table-wrap"><table><thead><tr><th>Age</th><th>Fleet 1 selectivity</th><th>Fleet 2 selectivity</th><th>${bioType} 1 weight (kg)</th><th>${bioType} 2 weight (kg)</th><th>${bioType} 1 maturity</th><th>${bioType} 2 maturity</th></tr></thead><tbody>${result.biology.map(b=>`<tr><td>${b.age}</td>${BIO_FIELDS.map(f=>`<td>${fmt(b[f],4)}</td>`).join('')}</tr>`).join('')}</tbody></table></div>`;
}
// Native MathML keeps the method reference readable offline, without a CDN.
function buildEquationReference() {
  const mi=s=>`<mi>${esc(s)}</mi>`,mo=s=>`<mo${s==='('||s===')'?' stretchy="false"':''}>${esc(s)}</mo>`,mn=s=>`<mn>${s}</mn>`,mt=s=>`<mtext>${esc(s).replace(/ /g,'&#160;')}</mtext>`;
  const row=s=>`<mrow>${s}</mrow>`,sub=(s,i)=>`<msub>${row(s)}${row(i)}</msub>`,sup=(s,i)=>`<msup>${row(s)}${row(i)}</msup>`;
  const both=(s,i,p)=>`<msubsup>${row(s)}${row(i)}${row(p)}</msubsup>`;
  const frac=(a,b)=>`<mfrac>${row(a)}${row(b)}</mfrac>`,sum=i=>sub(mo('∑'),mt(i)),at=(s,f)=>s+mo('(')+f+mo(')');
  const math=(s,label)=>`<math xmlns="http://www.w3.org/1998/Math/MathML" display="block" aria-label="${esc(label)}">${row(s)}</math>`;
  const F=mi('F'),zero=mn(0),x=mi('x'),Rref=both(mi('R'),mi('s'),mt('ref'));
  const phi=(closed=false)=>closed?both(mi('φ'),mi('s'),mi('C')):sub(mi('φ'),mi('s'));
  const Phi=(closed=false,index='sr')=>closed?both(mi('Φ'),mt(index),mi('C')):sub(mi('Φ'),mt(index));
  const Bstar=sup(mi('B'),mo('∗')),Y=at(mi('Y'),F),Req=both(mi('R'),mi('s'),mt('eq')),Ypr=sub(mt('YPR'),mi('s'));
  const domain=mt('0 ≤ F₁, F₂ ≤ 10'),ray=F+mo('=')+mi('g')+mi('c');
  const global=(closed=false,natal=false)=>frac(sum('s')+sub(mi('λ'),mi('s'))+at(natal?Phi(false,'ss'):phi(closed),F),sum('s')+sub(mi('λ'),mi('s'))+at(natal?Phi(false,'ss'):phi(closed),zero))+mo('=')+x;
  const originW=sub(mi('ω'),mi('s'));
  const regional=frac(sum('s')+originW+at(Phi(),F),sum('s')+originW+at(Phi(),zero));
  const objective=sum('s')+at(Req,F)+at(Ypr,F);
  const argmax=under=>`<munder>${mt('arg max')}${row(under)}</munder>`;
  const cards=[];
  const add=(id,title,category,formula,description,extra='')=>cards.push({id,title,category,formula,description,extra});
  const recWeight=sub(mi('λ'),mi('s'))+mo('=')+frac(Rref,sum('j')+sub(sup(mi('R'),mt('ref')),mi('j')));
  const eqWeight=sub(mi('λ'),mi('s'))+mo('=')+frac(mn(1),mn(2));
  const surveyWeight=sub(mi('λ'),mi('s'))+mo('=')+frac(sub(mi('Ī'),mi('s')),sub(mi('Ī'),mn(1))+mo('+')+sub(mi('Ī'),mn(2)));
  const catchWeight=sub(mi('λ'),mi('s'))+mo('=')+frac(sub(sup(mi('C'),mt('hist')),mi('s')),sub(sup(mi('C'),mt('hist')),mn(1))+mo('+')+sub(sup(mi('C'),mt('hist')),mn(2)));
  const weights=[['RecW','Recruitment weights',recWeight],['EqW','Equal population weights',eqWeight],['SurvW','Survey biomass weights',surveyWeight],['CatchW','Catch biomass weights',catchWeight]];
  for(const closed of [false,true])for(const [suffix,label,w] of weights) {
    const id=`${closed?'SpD':'SpE'}-${suffix}`;
    add(id,label+(closed?' · without movement':' · with movement'),'global',
      math(global(closed),`${id}: weighted spawning per recruit ratio equals target`)+math(w,`${id} weighting`),
      `${closed?'Use closed-region biology Φᶜ in both the fished and unfished calculations.':'Retain the selected movement in both the fished and unfished calculations.'} Solve one global F along the selected fishing ratio. ${suffix==='SurvW'?'Selected regional survey indices supply the weights; region s is used as the weight for natal population s.':suffix==='CatchW'?'Final historical catches by region supply the weights; region s is used as the weight for natal population s.':suffix==='RecW'?'Reference recruitment supplies the weights.':'Each natal population receives equal weight, regardless of its recruitment.'}`,
      math(Bstar+mo('=')+sum('s')+Rref+at(phi(closed),F),'Global biomass target')+'<p>The SPR weights λ and the recruitment used to construct the biomass target serve different purposes. Projections always retain the actual selected movement.</p>');
  }
  add('Natal-RecW','Count only natal-region spawning','global',math(global(false,true),'Natal-only recruitment-weighted SPR target')+math(recWeight,'Recruitment weighting'),
    'Retain movement, but count only Φₛₛ: spawning by natal population s in its birth region. Under natal homing all fish spawn at home, so this equals SpE-RecW.',
    math(Bstar+mo('=')+sum('s')+Rref+at(Phi(false,'ss'),F),'Natal-only biomass target'));
  add('Pop-Move','Population-specific SPR with movement','joint',
    math(frac(at(phi(),F),at(phi(),zero))+mo('=')+x+mt('   for s = 1, 2'),'Both natal populations meet the lifetime SPR target'),
    'Choose F₁ and F₂ jointly so each natal population retains x of its unfished lifetime spawning per recruit, summed over all spawning regions. The fishing ratio is free. An accepted joint solution need not exist.',
    math(sub(Bstar,mi('s'))+mo('=')+Rref+at(phi(),F),'Population biomass target'));
  add('Reg-Move','Regional SPR with movement','joint',
    math(at(sub(mi('Ψ'),mi('r')),F)+mo('=')+regional+mo('=')+x+mt('   for r = 1, 2'),'Both regions meet the regional SPR target'),
    'Choose F₁ and F₂ jointly. Each region counts spawning there from both natal origins, using the same selected origin weights ω in numerator and denominator. Default weights are normalized biological reference recruitment. Alternatives are equal origins, fixed historical survey/catch-share proxies, or a custom mix. Under complete natal homing positive weights cancel and this equals Pop-Move.',
    math(sub(Bstar,mi('r'))+mo('=')+sum('s')+Rref+at(Phi(),F),'Regional biomass target')+'<p>The selected weights define a regional spawning-output ratio for a standardized birth-origin mixture. Survey and catch shares are assigned to origins by birth region; they do not measure origin composition when fish mix. The biomass target still uses actual biological reference recruitment Rref, separate from ω. With default recruitment weights, both regional targets imply global RecW = x. Alternative weights imply that global target only when their mix matches reference recruitment, or under complete natal homing. For any selected mix, its matching global full-spawning ratio equals x when both regional ratios equal x.</p>');
  add('Pop-NoMove','Regional = population-specific SPR without movement','joint',
    math(frac(at(Phi(true,'rr'),sub(F,mi('r'))),at(Phi(true,'rr'),zero))+mo('=')+x+mt('   for r = 1, 2'),'Independent closed-region SPR targets'),
    'Set ordinary movement to zero for the BRP calculation. Each natal population remains in its own region, so the population-specific and regional targets coincide. Solve each region’s F independently, then apply those F values to the actual movement model.',
    math(sub(Bstar,mi('r'))+mo('=')+sub(sup(mi('R'),mt('ref')),mi('r'))+at(Phi(true,'rr'),sub(F,mi('r'))),'Closed-region biomass target'));
  add('Yield-SPR','Highest equilibrium yield subject to SPR constraints','yield',
    math(sup(F,mo('∗'))+mo('=')+argmax(domain)+Y,'Constrained yield optimum')+
    math(at(sub(mi('Ψ'),mi('r')),F)+mo('≥')+x+mt('   for r = 1, 2'),'Regional SPR constraints'),
    'Optimize F₁ and F₂ independently of the selected fishing ratio. Hold the selected regional origin mix fixed throughout the search. Default: Reg-Move’s equilibrium recruitment when an accepted solution is available, otherwise R₀. Equal, survey, catch and custom mixes use their selected fixed weights. Under natal homing regional and population SPR coincide.',
    math(Y+mo('=')+objective,'Equilibrium yield includes recruitment at each trial F')+'<p>Reference recruitment used in the SPR constraint is held fixed; equilibrium recruitment inside the yield objective is recalculated at each F. With fixed-mean recruitment, this is a fixed-recruitment yield comparison.</p>');
  add('Yield-Max','System MSY with BH / fixed-mean maximum yield','yield',
    math(sup(F,mo('∗'))+mo('=')+argmax(domain)+Y,'Unconstrained equilibrium yield optimum')+
    math(Y+mo('=')+objective,'Equilibrium yield uses equilibrium recruitment at every trial F'),
    'Maximize total equilibrium yield without SPR restrictions and without fixing the fishing ratio. With mean recruitment this is a fixed-recruitment benchmark. With Beverton–Holt it is system MSY, shown as “Yield-Max · system MSY”. Both regional F values are optimized jointly, but the components need not reach their individual maxima. No component-persistence constraints are imposed.');
  add('MSY-Pattern','MSY along the selected fishing pattern','yield',
    math(sup(mi('g'),mo('∗'))+mo('=')+argmax(mt('g ≥ 0; each g cᵣ ≤ 10'))+at(mi('Y'),mi('g')+mi('c')),'MSY with fixed relative fishing pattern'),
    'Require Beverton–Holt recruitment. Retain actual movement and biology, choose one global multiplier, and preserve the selected F₁ : F₂ ratio. The two regional fishing mortalities remain bounded by 10.');
  add('MSY-Joint','Check simultaneous component maxima / MSY-Balanced','yield',
    math(sub(mi('M'),mi('j'))+mo('=')+`<munder>${mo('max')}${row(domain)}</munder>`+at(sub(mi('Y'),mi('j')),F)+mt('   for j = 1, 2'),'Separate component maxima over the same F domain')+
    math(sub(F,mt('bal'))+mo('=')+argmax(domain)+`<munder>${mo('min')}${mt('j = 1, 2')}</munder>`+frac(at(sub(mi('Y'),mi('j')),F),sub(mi('M'),mi('j'))),'Balanced compromise maximizes the smaller fraction of the two maxima'),
    'The checkbox “Check simultaneous component maxima” produces the MSY-Balanced curve. Under NH, Yⱼ = Rⱼᵉᑫ × YPRⱼ groups catches by natal origin across both regions. Under MPO/MPD, Yⱼ is catch in fishing region j. Each component maximum is optimized over the same full F box.',
    math(`<munder>${mo('min')}${mt('j = 1, 2')}</munder>${frac(at(sub(mi('Y'),mi('j')),sub(F,mt('bal'))),sub(mi('M'),mi('j')))}${mo('≥')}${mn('0.9999')}`,'Simultaneous maxima found within numerical tolerance')+'<p>A minimum fraction of at least 0.9999 means simultaneous maxima were found within 0.01% relative tolerance. Otherwise this is a compromise, not a proof of nonexistence. This is a simultaneous-maxima test, rather than a game of fleet best responses. With no movement and freely chosen regional F, independent closed-population optima can be attained together.</p>');
  add('model-spawning','Spawning per recruit & age-specific biology','model',
    math(at(Phi(),F)+mo('=')+sum('a')+sub(sup(mi('n'),mt('spawn')),mt('sra'))+sub(mi('w'),mt('b(s,r),a'))+sub(mi('m'),mt('b(s,r),a')),'Lifetime spawning output of one recruit')+
    math(at(phi(),F)+mo('=')+sum('r')+at(Phi(),F),'Natal-population spawning per recruit sums all regions'),
    'nˢᵖᵃʷⁿ is the lifetime contribution of one recruit at the season-3 spawning midpoint, including the plus group. w is weight and m is maturity at age. For NH and MPO, biology b(s,r)=s follows natal origin; for MPD, b(s,r)=r follows the occupied region. Fishing selectivity belongs to each regional fleet.',
    math(sub(mi('n'),mt('+'))+mo('=')+sup(mo('(')+mi('I')+mo('−')+mi('A')+mo(')'),mo('−1'))+sub(mi('n'),mt('enter')),'Plus-group matrix sums repeated annual survival and movement')+'<p>A is the final-age annual survival-and-movement matrix. The inverse sums all years in the final age class; it does not truncate fish at the last listed age.</p>');
  add('model-seasonal','Mortality, movement & catch','model',
    math(sub(mi('Z'),mt('ra'))+mo('=')+mi('M')+mo('+')+sub(F,mi('r'))+sub(mi('q'),mt('ra')),'Total mortality at age')+
    math(sub(mi('C'),mt('sra,k'))+mo('=')+sub(mi('N'),mt('sra,k'))+frac(sub(F,mi('r'))+sub(mi('q'),mt('ra')),sub(mi('Z'),mt('ra')))+mo('(')+mn(1)+mo('−')+sup(mi('e'),mo('−')+sub(mi('Z'),mt('ra'))+mn('.25'))+mo(')')+sub(mi('w'),mt('b(s,r),a')),'Baranov catch biomass during a season by natal origin'),
    'Four seasons last 0.25 years each. Survival over duration d is exp(−Z d), followed by end-of-season movement. Spawning is at 0.625 years, before season-3 movement. NH replaces season-2 movement with complete return home. Catch weights use each fish’s origin biology for NH/MPO and occupied-region biology for MPD. Catch is summed across natal origins and ages.',
    math(sub(mi('P'),mi('k'))+mo('=')+`<mo>(</mo><mtable><mtr><mtd>${mn(1)+mo('−')+sub(mi('m'),mt('12,k'))}</mtd><mtd>${sub(mi('m'),mt('12,k'))}</mtd></mtr><mtr><mtd>${sub(mi('m'),mt('21,k'))}</mtd><mtd>${mn(1)+mo('−')+sub(mi('m'),mt('21,k'))}</mtd></mtr></mtable><mo>)</mo>`,'Season-specific movement probabilities from row region to column region')+
    '<p>Pₖ uses rows for origin region and columns for destination region, for seasonal transfer k = 1, 2, 3, 4. For column-vector numbers, survive first and then apply Pₖᵀ. The custom editor sets m₁₂,ₖ and m₂₁,ₖ independently for each season (percentages are divided by 100). Low movement uses 0.05 in each direction for all seasons; high directional movement uses 0.30 and 0.05; no movement uses zero. Under NH, replace the season-2 transfer with complete return to the natal region, regardless of its ordinary rates. Transfers are at 0.25, 0.50, 0.75 and 1.00 years; spawning at 0.625 years precedes the third transfer.</p>');
  add('model-fishing','Fishing patterns & global F normalization','model',
    math(ray+mt(';   ')+sub(mi('c'),mi('r'))+mo('=')+frac(sub(mi('a'),mi('r')),`<munder>${mo('max')}${mi('a′')}</munder>`+sum('j')+sub(mi('a'),mi('j'))+sub(mi('q'),mt('j a′'))),'WHAM-normalized fishing pattern')+
    math(sub(F,mt('global'))+mo('=')+`<munder>${mo('max')}${mi('a')}</munder>`+sum('r')+sub(F,mi('r'))+sub(mi('q'),mt('ra')),'WHAM global fishing mortality'),
    'aᵣ denotes the input relative fishing allocation; a′ is an age index in the normalization. qᵣₐ is fleet selectivity. Along this pattern g is global F, and the arbitrary scale of allocation cancels. The selected ratio describes fully selected F, rather than catch shares. Global SPR solves along this ray; regional/population joint SPR and free-F MSY choose their own ratio.');
  add('model-recruitment','Mean and Beverton–Holt recruitment','model',
    math(sub(sup(mi('R'),mt('next')),mi('u'))+mo('=')+frac(sub(mi('α'),mi('u'))+sub(mi('S'),mi('u')),mn(1)+mo('+')+sub(mi('β'),mi('u'))+sub(mi('S'),mi('u'))),'Beverton-Holt recruitment equation')+
    math(sub(mi('β'),mi('u'))+mo('=')+frac(sub(mi('α'),mi('u')),sub(mi('R'),mt('0u')))+mo('−')+frac(mn(1),sub(mi('S'),mt('0u'))),'Beverton-Holt common-slope calibration')+
    math(sub(mi('α'),mi('u'))+mo('=')+frac(mn(4)+mi('h')+sub(mi('R'),mt('0u')),mo('(')+mn(1)+mo('−')+mi('h')+mo(')')+sub(mi('S'),mt('0u')))+mt(';   ')+sub(mi('β'),mi('u'))+mo('=')+frac(mn(5)+mi('h')+mo('−')+mn(1),mo('(')+mn(1)+mo('−')+mi('h')+mo(')')+sub(mi('S'),mt('0u'))),'Equal-steepness Beverton-Holt calibration'),
    'The reproductive unit u is a natal population under NH and a spawning region under MPO/MPD. S₀ is its unfished spawning biomass at R₀. The mean model sets R=R₀ independently of spawning biomass. BH equilibrium solves these recruitment equations together with the spawning-per-recruit model.',
    math(sub(sup(mi('R'),mt('ref')),mi('s'))+mo('=')+at(Req,sup(F,mo('∗'))),'BH SPR reference recruitment must agree with actual equilibrium recruitment')+'<p>BH SPR reference recruitment agrees with the actual-model equilibrium at the target F. Recruitment-dependent targets use a damped fixed-point iteration; a regional target with a fixed alternative origin mix obtains biological reference recruitment directly at its solved F. MSY recalculates equilibrium recruitment at every candidate F. MSY recruitment-weighted diagnostics use fixed R₀; displayed regional diagnostics use the selected origin mix. SPR and biomass depletion are different statistics.</p>');
  add('model-surveys','Eight surveys, ensemble weights & catch weights','model',
    math(sub(mi('t'),mi('k'))+mo('=')+frac(mi('k')+mo('−')+mn('.5'),mn(4))+mt('   k = 1, 2, 3, 4'),'Survey midpoints within the year')+
    math(sub(mi('Ī'),mi('r'))+mo('=')+frac(sum('k ∈ Kᵣ')+sub(mi('I'),mt('rk')),mo('|')+sub(mi('K'),mi('r'))+mo('|')),'Ensemble regional index averages selected surveys'),
    'Iᵣₖ is selected biomass × survey catchability 0.1 at a seasonal midpoint, using the standard model survey selectivity. All surveys sample the same final historical year. Kᵣ is the set checked for region r. With ensemble disabled use the sum instead of the mean. Normalize the two regional indices to obtain survey weights. Historical catch weights normalize the two final-year regional catches. At least one survey must be selected per region.');
  add('model-projection','Biomass feedback & recruitment variation','model',
    math(sub(F,mi('t'))+mo('=')+sup(F,mo('∗'))+mt(' clip')+mo('(')+frac(sub(mi('B'),mt('t−1')),Bstar)+mo(',')+mn(0)+mo(',')+mn(1)+mo(')'),'Annual linear biomass-feedback fishing rule')+
    math(sub(mi('R'),mt('t+1,u'))+mo('=')+sub(sup(mi('R'),mt('model')),mt('t+1,u'))+sup(mi('e'),sub(mi('ε'),mt('tu'))+mo('−')+frac(sup(mi('σ'),mn(2)),mn(2))),'Mean-corrected stochastic recruitment multiplier'),
    'B is previous-year regional SSB for regional rules, natal-origin SSB for Pop-Move, summed natal-region spawning for Natal-RecW, and total regional SSB for other global rules. The ratio is componentwise for population/regional rules; fleet r uses component r. Fixed-F projections omit the feedback ramp. Yield and MSY benchmark curves always hold optimized F constant.',
    '<p>ε has normal distribution with mean 0 and variance σ². Recruitment draws are independent between the two reproductive units and paired across reference-point comparisons. The default 50 historical years condition the common starting state; the GUI checks SSB and age structure against analytical equilibrium rather than assuming 50 years always stabilizes the population.</p>');
  $('equationNotation').innerHTML='<dl><div><dt>s / r / a</dt><dd>Natal population / occupied region / age. There are two natal origins and two regions.</dd></div><div><dt>x</dt><dd>SPR target as a fraction: the default 40% is x = 0.40. A shared SPR target does not imply the same biomass depletion.</dd></div><div><dt>Φₛᵣ(F), φₛ(F)</dt><dd>Lifetime spawning biomass in region r from one recruit of natal population s; φₛ is the sum over regions.</dd></div><div><dt>Φᶜ, F = 0</dt><dd>Superscript C means movement omitted from the BRP calculation. F = 0 is its corresponding unfished baseline.</dd></div><div><dt>R<sup>ref</sup>, R<sup>eq</sup>(F), R₀</dt><dd>SPR reference recruitment, equilibrium recruitment at trial F, and unfished / mean recruitment. These have distinct roles.</dd></div><div><dt>ωₛ</dt><dd>Selected regional birth-origin weight, identical in fished and unfished calculations. Default: normalized Rref; alternatives: equal, historical survey/catch-share proxies, or custom. It is separate from biological recruitment used for biomass.</dd></div><div><dt>F = (F₁,F₂), B*, YPR</dt><dd>Fully selected regional fishing mortalities (yr⁻¹), rule-specific SSB target, and lifetime catch biomass per recruit.</dd></div></dl>';
  $('equationCards').innerHTML=cards.map(card=>{
    const styleRule=card.id==='MSY-Joint'?'MSY-Balanced':card.id;
    return `<details class="equation-card" data-equation="${card.id}" data-category="${card.category}" data-search="${esc((card.id+' '+card.title+' '+card.description).toLowerCase())}"><summary><span class="equation-method">${COLORS[styleRule]?ruleSwatch(styleRule):'<span class="equation-model-mark">ƒ</span>'}<span><strong>${esc(card.id)}</strong><small>${esc(card.title)}</small></span></span><span class="equation-category-label">${{global:'GLOBAL SPR',joint:'JOINT SPR',yield:'YIELD / MSY',model:'MODEL'}[card.category]}</span></summary><div class="equation-card-body"><div class="equation-formulas">${card.formula}</div><p>${card.description}</p>${card.extra?`<details class="equation-more"><summary>Targets & interpretation</summary>${card.extra}</details>`:''}</div></details>`;
  }).join('');
}
function updateEquationReference() {
  const cfg=getSettings();
  $('equationContext').textContent=`Current controls: x = ${(cfg.target/100).toFixed(3)} (${cfg.target}%) · F₁ : F₂ = ${cfg.fishing_share1} : ${100-cfg.fishing_share1} · ${describe(cfg)} · Regional mix: ${regionalWeightLabel(cfg)}. Definitions below describe these controls; calculate to update numerical results.`;
  filterEquationReference();
}
function filterEquationReference() {
  const category=$('equationCategory').value,query=$('equationSearch').value.trim().toLowerCase();
  const selected=new Set([...selectedRules(),...selectedBenchmarks(),...selectedMSY()]);
  let count=0;
  document.querySelectorAll('.equation-card').forEach(card=>{
    const match=(category==='all'||category==='selected'&&selected.has(card.dataset.equation)||card.dataset.category===category)&&(!query||card.dataset.search.includes(query));
    card.hidden=!match;if(match)count++;
    if(query&&match)card.open=true;
  });
  $('equationCount').textContent=`${count} ${count===1?'method':'methods'}`;
  $('equationEmpty').hidden=count>0;
}
function openBiology() {
  editingProfile=$('life_history').value;
  biologyDraftIsDefault=!customBiology[editingProfile];
  biologyDraft=structuredClone(customBiology[editingProfile]||biologyDefaults[editingProfile]);
  $('biologyError').hidden=true;
  $('biologyTitle').textContent=`Age-specific inputs · ${editingProfile==='short'?'10':'30'} ages`;
  const type=getSettings().structure==='MPD'?'Region':'Natal population';
  $('biologyLabels').textContent=`Fleet 1 fishes in Region 1; Fleet 2 fishes in Region 2. ${type} 1 and ${type.toLowerCase()} 2 supply the two weight and maturity curves. The last age is a plus group.`;
  for(const r of [1,2]) { $(`weightHeader${r}`).textContent=`${type} ${r} weight (kg)`; $(`maturityHeader${r}`).textContent=`${type} ${r} maturity`; }
  renderBiologyTable();$('biologyDialog').showModal();
}
function renderBiologyTable() {
  $('biologyTable').querySelector('tbody').innerHTML=biologyDraft.weight1.map((_,i)=>`<tr><td>${i+1}${i===biologyDraft.weight1.length-1?'+':''}</td>${BIO_FIELDS.map(field=>`<td><input type="number" min="0" max="${field.startsWith('weight')?1000000:1}" step="any" required aria-label="${field} age ${i+1}" data-field="${field}" data-age="${i}" value="${Number(biologyDraft[field][i].toPrecision(12))}"></td>`).join('')}</tr>`).join('');
  renderBiologyPreview();
}
function readBiologyDraft() {
  const next=Object.fromEntries(BIO_FIELDS.map(f=>[f,Array(biologyDraft.weight1.length).fill(0)]));
  for(const input of $('biologyTable').querySelectorAll('input')) {
    if(!input.reportValidity())return null;
    next[input.dataset.field][Number(input.dataset.age)]=Number(input.value);
  }
  for(const r of [1,2]) {
    if(!next[`selectivity${r}`].some(x=>x>0))throw new Error(`Fleet ${r} must select at least one age.`);
    if(!next[`weight${r}`].some((w,i)=>w*next[`maturity${r}`][i]>0))throw new Error(`Biology type ${r} needs positive weight and maturity at some age.`);
  }
  return next;
}
function renderBiologyPreview() {
  const titles={selectivity:'Fishing selectivity',weight:'Weight (kg)',maturity:'Maturity'};
  $('biologyPreview').innerHTML=['selectivity','weight','maturity'].map(metric=>{
    const n=biologyDraft[metric+'1'].length, max=metric==='weight'?Math.max(...biologyDraft.weight1,...biologyDraft.weight2,1e-9)*1.05:1;
    let svg=`<svg viewBox="0 0 270 124" role="img" aria-label="${titles[metric]} at age"><text x="12" y="17" font-size="10" fill="#4e6380">${titles[metric]}</text><line x1="27" x2="255" y1="98" y2="98" stroke="#c5d0e1"/><text x="8" y="101" font-size="8" fill="#7b8ca4">0</text><text x="5" y="38" font-size="8" fill="#7b8ca4">${compact(max)}</text><text x="27" y="115" font-size="8" fill="#7b8ca4">Age 1</text><text x="255" y="115" text-anchor="end" font-size="8" fill="#7b8ca4">${n}+</text>`;
    for(const r of [1,2])svg+=`<polyline points="${biologyDraft[metric+r].map((v,i)=>`${27+i/(n-1)*228},${98-v/max*66}`).join(' ')}" fill="none" stroke="${r===1?'#355d91':'#b8673f'}" stroke-width="2"/>`;
    return svg+'</svg>';
  }).join('');
}
function biologyError(error) { $('biologyError').textContent=error.message;$('biologyError').hidden=false; }
function saveAgeTable() {
  try {
    const draft=readBiologyDraft();if(!draft)return;
    customBiology[editingProfile]=biologyDraftIsDefault?null:draft;$('biologyDialog').close();markDirty();
  } catch(e){biologyError(e);}
}
function exportAgeTable() {
  try {
    const draft=readBiologyDraft();if(!draft)return;
    const csv=['age,'+BIO_FIELDS.join(','),...draft.weight1.map((_,i)=>[i+1,...BIO_FIELDS.map(f=>draft[f][i])].join(','))].join('\n');
    const url=URL.createObjectURL(new Blob([csv],{type:'text/csv'}));
    const anchor=document.createElement('a');anchor.href=url;anchor.download=`biology-${editingProfile}.csv`;anchor.click();setTimeout(()=>URL.revokeObjectURL(url),1000);
  } catch(e){biologyError(e);}
}
async function importAgeTable(event) {
  const file=event.target.files[0];if(!file)return;
  try {
    const text=await file.text(), lines=text.replace(/^\uFEFF/,'').trim().split(/\r?\n/).filter(x=>x.trim());
    const split=line=>line.split(',').map(x=>x.trim().replace(/^"|"$/g,''));
    const header=split(lines.shift()), indices=BIO_FIELDS.map(f=>header.indexOf(f)), ageCol=header.indexOf('age');
    if(ageCol<0||indices.some(i=>i<0))throw new Error('CSV needs age, selectivity1, selectivity2, weight1, weight2, maturity1 and maturity2 columns. Export the age table for a template.');
    if(lines.length!==biologyDraft.weight1.length)throw new Error(`CSV must contain ${biologyDraft.weight1.length} age rows for the selected life-history profile.`);
    const next=Object.fromEntries(BIO_FIELDS.map(f=>[f,[]]));
    lines.forEach((line,i)=>{
      const row=split(line);
      if(Number(row[ageCol].replace('+',''))!==i+1)throw new Error('Age rows must be in order, starting at age 1.');
      BIO_FIELDS.forEach((f,j)=>{
        if(row[indices[j]]==null||row[indices[j]]==='')throw new Error(`Missing ${f} at age ${i+1}.`);
        const value=Number(row[indices[j]]), max=f.startsWith('weight')?1000000:1;
        if(!Number.isFinite(value)||value<0||value>max)throw new Error(`Invalid ${f} at age ${i+1}.`);
        next[f].push(value);
      });
    });
    biologyDraft=next;biologyDraftIsDefault=false;renderBiologyTable();$('biologyError').hidden=true;
  } catch(e){biologyError(e);}finally{event.target.value='';}
}
function movementRates(cfg=getSettings()) {
  if(cfg.movement==='custom'&&cfg.seasonal_movement)return structuredClone(cfg.seasonal_movement);
  const rates={high:[30,5],low:[5,5],none:[0,0]}[cfg.movement]||[30,5];
  return Array.from({length:4},()=>({m12:rates[0],m21:rates[1]}));
}
function openMovementEditor(fromSelect=false) {
  movementEditorPreviousMode=fromSelect?lastMovementMode:$('movement').value;
  movementDraft=movementRates(getSettings());
  $('movementEditorPreset').value=['high','low','none'].includes($('movement').value)?$('movement').value:'high';
  $('seasonalMovementError').hidden=true;
  renderMovementEditor();$('seasonalMovementDialog').showModal();
}
function renderMovementEditor() {
  const nh=getSettings().structure==='NH';
  $('seasonalMovementHoming').textContent=nh?'Natal homing: the season-2 transfer is fixed at complete return to each fish’s natal region. Customize seasons 1, 3 and 4.':'Metapopulation: customize all four seasonal transfers. Fish spawn in their occupied region at 0.625 years, before the season-3 transfer.';
  $('seasonalMovementTable').querySelector('tbody').innerHTML=movementDraft.map((m,k)=>`<tr><th>End of season ${k+1}</th><td>${(k+1)/4}</td>${['m12','m21'].map(d=>`<td><input type="number" min="0" max="100" step="any" required data-season="${k}" data-direction="${d}" aria-label="Season ${k+1} ${d==='m12'?'Region 1 to 2':'Region 2 to 1'} percent" value="${nh&&k===1?'':m[d]}" ${nh&&k===1?'disabled placeholder="Natal return"':''}></td>`).join('')}<td class="movement-rule">${nh&&k===1?'Complete natal return':'Ordinary transfer'}</td></tr>`).join('');
  renderMovementPreview();
}
function movementCards(schedule, nh, prefix) {
  return schedule.map((m,k)=>`<article><h4>Season ${k+1} <span>${(k+1)/4} yr</span></h4>${nh&&k===1?'<div class="homing-preview"><strong>100% natal return</strong><span>All fish return to their birth region</span></div>':`<svg viewBox="0 0 224 124" role="img" aria-label="Season ${k+1}: ${m.m12}% from Region 1 to 2 and ${m.m21}% from Region 2 to 1"><defs><marker id="${prefix}Blue${k}" markerWidth="6" markerHeight="6" refX="5" refY="3" orient="auto"><path d="M0 0L6 3L0 6" fill="#355d91"/></marker><marker id="${prefix}Copper${k}" markerWidth="6" markerHeight="6" refX="5" refY="3" orient="auto"><path d="M0 0L6 3L0 6" fill="#aa4c2e"/></marker></defs><rect x="10" y="44" width="44" height="33" rx="2" fill="#e8eef8" stroke="#a0b3cd"/><rect x="170" y="44" width="44" height="33" rx="2" fill="#f6e8dc" stroke="#caa487"/><text x="32" y="65" text-anchor="middle" font-size="12" fill="#253953">R1</text><text x="192" y="65" text-anchor="middle" font-size="12" fill="#7b4228">R2</text><path d="M32 42C66 8 158 8 192 42" fill="none" stroke="#355d91" stroke-width="${1+3*m.m12/100}" marker-end="url(#${prefix}Blue${k})"/><path d="M192 79C158 112 66 112 32 79" fill="none" stroke="#aa4c2e" stroke-width="${1+3*m.m21/100}" marker-end="url(#${prefix}Copper${k})"/><text x="112" y="17" text-anchor="middle" font-size="11" fill="#355d91">${fmt(m.m12,1)}%</text><text x="112" y="120" text-anchor="middle" font-size="11" fill="#aa4c2e">${fmt(m.m21,1)}%</text></svg>`}</article>`).join('');
}
function renderMovementPreview() {
  $('seasonalMovementPreview').innerHTML=movementCards(movementDraft,getSettings().structure==='NH','editor');
}
function readMovementDraft() {
  const draft=structuredClone(movementDraft);
  for(const input of $('seasonalMovementTable').querySelectorAll('input')) {
    if(input.disabled)continue;
    if(!input.reportValidity())return null;
    const value=Number(input.value);
    if(!Number.isFinite(value)||value<0||value>100)throw new Error('Each directional probability must be between 0 and 100%.');
    draft[Number(input.dataset.season)][input.dataset.direction]=value;
  }
  return draft;
}
function saveMovementEditor() {
  try {
    const draft=readMovementDraft();if(!draft)return;
    customSeasonalMovement=draft;$('movement').value='custom';lastMovementMode='custom';
    $('seasonalMovementDialog').close();markDirty();
  } catch(error) {$('seasonalMovementError').textContent=error.message;$('seasonalMovementError').hidden=false;}
}
function cancelMovementEditor() {
  $('movement').value=movementEditorPreviousMode;
  $('seasonalMovementDialog').close();markDirty();
}
function showMovement() {
  const cfg=getSettings(), nh=cfg.structure==='NH';
  $('movementTitle').textContent=nh?'Natal homing across four seasons':'Metapopulation movement across four seasons';
  $('movementModalText').textContent=`${describe(cfg)}. This diagram uses your current movement settings.`;
  $('movementSchedule').innerHTML=movementCards(movementRates(cfg),nh,'view');
  $('movementDialog').showModal();
}

function selectResultView(view) {
  document.querySelectorAll('[data-view]').forEach(b=>{const active=b.dataset.view===view;b.classList.toggle('active',active);b.setAttribute('aria-selected',String(active));$(b.dataset.view).hidden=!active;});
  if(view==='dynamicsView')renderCharts();
  const referenceView=view==='equationsView';
  document.querySelector('.summary-strip').hidden=referenceView;
  document.querySelector('.results-heading').hidden=referenceView;
  document.querySelector('.status-line').hidden=referenceView;
  $('historyStatus').hidden=referenceView;
  if(view==='equationsView')updateEquationReference();
  updateResultHeading();
}
function updateResultHeading() {
  $('resultTitle').textContent='Population dynamics & catch';
  $('runDescription').textContent=result?describe(result.settings):describe(getSettings());
}

buildRules(); buildEquationReference(); renderCharts();
$('brpTab').addEventListener('click',()=>sideTab(false)); $('settingsTab').addEventListener('click',()=>sideTab(true)); $('openSettings').addEventListener('click',()=>sideTab(true));
$('toggleAll').addEventListener('click',()=>{const select=selectedRules().length!==document.querySelectorAll('input[name=rule]').length;document.querySelectorAll('input[name=rule]').forEach(x=>x.checked=select);markDirty();});
document.querySelectorAll('input[name=benchmark]').forEach(input=>input.addEventListener('change',markDirty));
document.querySelectorAll('input[name=msy]').forEach(input=>input.addEventListener('change',markDirty));
$('enableMSY').addEventListener('click',()=>{$('recruitment').value='sr';markDirty();});
$('target').addEventListener('input',markDirty);
$('targetPreset').addEventListener('change',()=>{
  if($('targetPreset').value==='custom') { $('target').focus();$('target').select(); }
  else { $('target').value=$('targetPreset').value;markDirty(); }
});
$('modelControls').addEventListener('submit',event=>event.preventDefault());
$('modelControls').addEventListener('input',markDirty);
$('modelControls').addEventListener('change',markDirty);
$('regional_weighting').addEventListener('change',markDirty);
$('regional_share1').addEventListener('input',markDirty);
$('fishingPreset').addEventListener('change',()=>{if($('fishingPreset').value!=='custom')$('fishing_share1').value=$('fishingPreset').value;markDirty();});
$('fishing_share1').addEventListener('input',()=>{$('fishingPreset').value='custom';markDirty();});
$('reset').addEventListener('click',()=>{customBiology={short:null,long:null};applySettings(defaults);markDirty();});
$('audienceScenario').addEventListener('change',previewAudienceScenario);
$('loadAudienceScenario').addEventListener('click',()=>loadAudienceScenario($('audienceScenario').value));
$('copyAudienceLink').addEventListener('click',copyAudienceLink);
$('runButton').addEventListener('click',calculate);
$('download').addEventListener('click',async()=>{
  if(!resultRunId||!result)return;
  try {
    if(window.brpBrowser)await window.brpBrowser.download(resultRunId);
    else location.href=`/api/runs/${resultRunId}/download`;
  } catch(error) {$('error').textContent=`Export failed: ${error.message}`;$('error').hidden=false;}
});
$('populationMetric').addEventListener('change',renderCharts);
document.querySelectorAll('[data-view]').forEach(button=>button.addEventListener('click',()=>selectResultView(button.dataset.view)));
$('openEquations').addEventListener('click',()=>selectResultView('equationsView'));
$('equationSearch').addEventListener('input',filterEquationReference);
$('equationCategory').addEventListener('change',filterEquationReference);
$('movementButton').addEventListener('click',showMovement);$('showMovementRight').addEventListener('click',showMovement);
$('customizeMovement').addEventListener('click',()=>openMovementEditor());
$('movement').addEventListener('change',()=>{if($('movement').value==='custom')openMovementEditor(true);else lastMovementMode=$('movement').value;});
for(const id of ['closeSeasonalMovement','cancelSeasonalMovement'])$(id).addEventListener('click',cancelMovementEditor);
$('seasonalMovementDialog').addEventListener('cancel',event=>{event.preventDefault();cancelMovementEditor();});
$('saveSeasonalMovement').addEventListener('click',saveMovementEditor);
$('fillMovementPreset').addEventListener('click',()=>{movementDraft=movementRates({movement:$('movementEditorPreset').value});renderMovementEditor();});
$('copyFirstMovement').addEventListener('click',()=>{const draft=readMovementDraft();if(!draft)return;movementDraft=draft.map((m,k)=>getSettings().structure==='NH'&&k===1?m:structuredClone(draft[0]));renderMovementEditor();});
$('seasonalMovementTable').addEventListener('input',event=>{const input=event.target;if(input.matches('input')&&input.checkValidity()){movementDraft[Number(input.dataset.season)][input.dataset.direction]=Number(input.value);renderMovementPreview();}});
$('editBiology').addEventListener('click',openBiology);
for(const id of ['closeBiology','cancelBiology'])$(id).addEventListener('click',()=>$('biologyDialog').close());
$('saveBiology').addEventListener('click',saveAgeTable);$('downloadBiology').addEventListener('click',exportAgeTable);
$('importBiology').addEventListener('change',importAgeTable);
$('resetBiologyDraft').addEventListener('click',()=>{biologyDraft=structuredClone(biologyDefaults[editingProfile]);biologyDraftIsDefault=true;renderBiologyTable();$('biologyError').hidden=true;});
$('biologyTable').addEventListener('input',event=>{const input=event.target;if(input.matches('input')&&input.checkValidity()){biologyDraftIsDefault=false;biologyDraft[input.dataset.field][Number(input.dataset.age)]=Number(input.value);renderBiologyPreview();}});
$('closeMovement').addEventListener('click',()=>$('movementDialog').close());
$('movementDialog').addEventListener('click',e=>{if(e.target===$('movementDialog')){const r=$('movementDialog').getBoundingClientRect();if(e.clientX<r.left||e.clientX>r.right||e.clientY<r.top||e.clientY>r.bottom)$('movementDialog').close();}});
let resizeTimer;window.addEventListener('resize',()=>{clearTimeout(resizeTimer);resizeTimer=setTimeout(renderCharts,120);});
window.addEventListener('hashchange',()=>{
  const id=new URLSearchParams(location.hash.slice(1)).get('scenario');
  if(id&&id!==activeAudienceScenario?.id)loadAudienceScenario(id);
  else if(location.hash==='#equations')selectResultView('equationsView');
});
modelRequest('/api/defaults').then(r=>r.json()).then(data=>{
  defaults=data.defaults;biologyDefaults=data.biology_defaults;audienceScenarios=data.audience_scenarios||[];
  $('audienceScenario').innerHTML=audienceScenarios.map(s=>`<option value="${esc(s.id)}">${esc(s.title)}</option>`).join('');
  const requestedScenario=new URLSearchParams(location.hash.slice(1)).get('scenario');
  const opening=audienceScenarios.find(s=>s.id===requestedScenario)||audienceScenarios.find(s=>s.id===data.opening_scenario)||audienceScenarios[0];
  if(opening)loadAudienceScenario(opening.id,false);else applySettings(defaults);
  updateEquationReference();
  if(location.hash==='#equations') {selectResultView('equationsView');setStatus('Viewing equation definitions · calculate a scenario to populate Dynamics.','');}
  else calculate();
}).catch(error=>{$('error').textContent=`Cannot load the model: ${error.message}`;$('error').hidden=false;});
