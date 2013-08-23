<?php if (isset($DOKU_TPL)==FALSE) $DOKU_TPL = DOKU_TPL; if (isset($DOKU_TPLINC)==FALSE) $DOKU_TPLINC = DOKU_TPLINC; ?>
<?php
/**
 * DokuWiki Default Template
 *
 * This is the template you need to change for the overall look
 * of DokuWiki.
 *
 * @link   http://wiki.splitbrain.org/wiki:tpl:templates
 * @author Andreas Gohr <andi@splitbrain.org>
 * @author Esther Brunner <wikidesign@gmail.com>
 * @author Oscar M. Lage <r0sk10@gmail.com>
 */

// must be run from within DokuWiki
if (!defined('DOKU_INC')) die();

// include functions that provide sidebar and tabs functionality
@require_once(dirname(__FILE__).'/functions.php');
tpl_checkColor();

?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
 "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="<?php echo $conf['lang']?>"
 lang="<?php echo $conf['lang']?>" dir="<?php echo $lang['direction']?>">
 
<head>
  <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
  <title>
    <?php tpl_pagetitle()?> · <?php echo strip_tags($conf['title'])?>
  </title>

  <?php tpl_metaheaders()?>

  <link rel="shortcut icon" href="<?php echo $DOKU_TPL?>images/favicon.ico" />

  <?php /*old includehook*/ @include(dirname(__FILE__).'/meta.html')?>
</head>

<body>
<?php /*old includehook*/ @include(dirname(__FILE__).'/topheader.html')?>
<div class="all" id="<?php tpl_classID()?>">
  <?php html_msgarea()?>

  <div class="header">
    <div class="bar-top" >
      <div class="bar-left" >
	    
        <a <?php echo $tgt?> href="http://www.opentelemac.org/"><img src="<?php echo $DOKU_TPL; ?>images/OTM_menu_icons/OTM_Home-icon_15pix_212-118-0.png"/> HOME </a>
		<a <?php echo $tgt?> href="http://www.opentelemac.org/index.php/contact2"><img src="<?php echo $DOKU_TPL; ?>images/OTM_menu_icons/OTM_Mail-icon_15pix_212-118-0.png"/>CONTACT </a>
		<?php tpl_searchform()?>
		</div>		
	  <div class="bar-right" >
	    <a <?php echo $tgt?> href="http://www.opentelemac.org/index.php/community">COMMUNITY </a>
	    <a <?php echo $tgt?> href="http://www.opentelemac.org/index.php/user-conference">CONFERENCE </a>
	    <a <?php echo $tgt?> href="http://www.opentelemac.org/index.php/download">DOWNLOAD </a>
	    <a <?php echo $tgt?> href="http://docs.opentelemac.org/">DOXY DOCS </a>		
	    <a <?php echo $tgt?> href="http://www.opentelemac.org/index.php/kunena">FORUM </a>
        <a <?php echo $tgt?> href="http://wiki.opentelemac.org/doku.php" style="color:#333;background:#ffd1a3;padding:10px;">WIKI</a>	
      </div>
    </div>	  
  
    <div class="logo">
      <?php tpl_link(wl(),$conf['title'],'name="dokuwiki__top" id="dokuwiki__top" accesskey="h" title="[ALT+H]"')?> 
       <div class = "header1"> The mathematically superior suite of solvers </div>
	</div>
	  
  </div>

  <?php /*old includehook*/ @include(dirname(__FILE__).'/header.html')?>

  <div class="main">
  
    <?php if($conf['breadcrumbs']){?>
    <div class="breadcrumbs">
      <?php tpl_breadcrumbs()?>
    </div>
    <?php }?>
  
    <?php if($conf['youarehere']){?>
    <div class="breadcrumbs">
      <?php tpl_youarehere()?>
    </div>
    <?php }?>
	


    <?php /*old includehook*/ @include(dirname(__FILE__).'/pageheader.html')?>
    
    <?php tpl_tabs()?>
  
    <div class="page">
        
      <!-- wikipage start -->
      <?php tpl_content()?>
      <!-- wikipage stop -->
	  
	  <form class="button" method="get" action="<?php echo wl($ID)?>">
        <div class="no">
          <input type="submit" value="Add/Remove selection" class="button" style="margin-top:10px;"/>
          <input type="hidden" name="do" value="addtobook" />
          <input type="hidden" name="id" value="<?php echo $ID?>" />
        </div>
      </form>
      
      <?php if ($INFO['exists'] && ($ACT == 'show') && $INFO['perm']
        && tpl_getConf('showpageinfo')){?>
      <div class="meta">
        <?php tpl_pageinfo()?>
      </div>
      <?php }?>
	  

	  <?php flush()?>
      
      <?php /*old includehook*/ @include(dirname(__FILE__).'/pagefooter.html')?>
  
      <?php if ($INFO['editable']){?>
      <?php }?>
    </div><!-- page -->
    
	
   <div class="bar" id="bar__bottom">
      <div class="bar-left" id="bar__bottomleft">
		<a href="<?php global $REV,$ID; echo exportlink($ID, 'pdf', "rev=$REV")?>">
           <img src="<?php echo DOKU_BASE?>lib/images/fileicons/pdf.png" alt="PDF Export" />
        </a>	  
        <?php tpl_button('edit')?>
        <?php tpl_button('history')?>
        <?php tpl_button('revert')?>
      </div>
      <div class="bar-right" id="bar__bottomright">
        <?php tpl_button('subscribe')?>
        <?php tpl_button('media')?>
        <?php tpl_button('admin')?>
        <?php tpl_button('profile')?>
        <?php tpl_button('login')?>
        <?php tpl_button('index')?>
        <?php tpl_button('top')?>&#160;
       </div>
    </div>
  </div><!-- main -->
</div><!-- all -->
  
<div class="footer-outer">
   <div class="footer">
        <script type="text/javascript">
        jQuery.noConflict();
		jQuery(document).ready(function($)
			{
				$('#ahgalleryOTconsortium ul.hover_block0 li.item').hover(function(){
					$(this).find('img.overlay').animate({top:'60px'},{queue:false,duration:500});
				}, function(){
					$(this).find('img.overlay').animate({top:'5px'},{queue:false,duration:500});
				});
		});
		</script>
<div id="ahgalleryOTconsortium">
	<ul class="hover_block0 bottom_block">
	    <!--galleryEntry 1 -->
		<li class="item">
			<a class="teaser" style="background-color:transparent !important;  font-weight: normal; text-decoration: none;" href="http://www.arteliagroup.com/" target="_blank">
		    <img class="overlay" src="http://www.opentelemac.org/modules/mod_animate_hover/images/Sogreah-Artelia.jpg" alt="" />
			<span style="color:grey; font-size: 14px; font-weight: bold; line-height: 18px; font-family: arial; margin-bottom: 0.5em; margin-top: 0.5em; display: block; text-align: center;">Artelia</span>
			<span style="color:white; display: block; font-size: 12px; font-weight: normal; font-family: arial; margin-bottom: 0.5em; margin-top: 0.5em;"></span>
			</a>
		</li>
		<!--galleryEntry 2 -->
		<li class="item">
			<a class="teaser" style="background-color:transparent !important;  font-weight: normal; text-decoration: none;" href="http://www.baw.de/de/index.php.html" target="_blank">
		    <img class="overlay" src="http://www.opentelemac.org/modules/mod_animate_hover/images/logo_baw.png" alt="" />
			<span style="color:grey; font-size: 14px; font-weight: bold; line-height: 18px; font-family: arial; margin-bottom: 0.5em; margin-top: 0.5em; display: block; text-align: center;">BundesAnstalt für Wasserbau</span>
			<span style="color:white; display: block; font-size: 12px; font-weight: normal; font-family: arial; margin-bottom: 0.5em; margin-top: 0.5em;"></span>
			</a>
		</li>
		<!--galleryEntry 3 -->
		<li class="item">
			<a class="teaser" style="background-color:transparent !important;  font-weight: normal; text-decoration: none;" href="http://www.cetmef.equipement.gouv.fr/" target="_blank">
		    <img class="overlay" src="http://www.opentelemac.org/modules/mod_animate_hover/images/logo_cetmef_v2.png" alt="" />
			<span style="color:grey; font-size: 14px; font-weight: bold; line-height: 18px; font-family: arial; margin-bottom: 0.5em; margin-top: 0.5em; display: block; text-align: center;">CETMEF</span>
			<span style="color:white; display: block; font-size: 12px; font-weight: normal; font-family: arial; margin-bottom: 0.5em; margin-top: 0.5em;"></span>
		    </a>
		</li>
		<!--galleryEntry 4 -->
		<li class="item">
			<a class="teaser" style="background-color:transparent !important;  font-weight: normal; text-decoration: none;" href="http://www.stfc.ac.uk/About%20STFC/45.aspx" target="_blank">
		    <img class="overlay" src="http://www.opentelemac.org/modules/mod_animate_hover/images/logo_Daresbury_v3.gif" alt="" />
		    <span style="color:grey; font-size: 14px; font-weight: bold; line-height: 18px; font-family: arial; margin-bottom: 0.5em; margin-top: 0.5em; display: block; text-align: center;">Daresbury Laboratory</span>
			<span style="color:white; display: block; font-size: 12px; font-weight: normal; font-family: arial; margin-bottom: 0.5em; margin-top: 0.5em;"></span>
			</a>
		</li>
		<!--galleryEntry 5 -->
		<li class="item">
			<a class="teaser" style="background-color:transparent !important;  font-weight: normal; text-decoration: none;" href="http://research.edf.com/research-and-innovation-44204.html&amp;tab=44205" target="_blank">
		    <img class="overlay" src="http://www.opentelemac.org/modules/mod_animate_hover/images/logo_edfR&D.jpg" alt="" />
		    <span style="color:grey; font-size: 14px; font-weight: bold; line-height: 18px; font-family: arial; margin-bottom: 0.5em; margin-top: 0.5em; display: block; text-align: center;">EDF R&D</span>
			<span style="color:white; display: block; font-size: 12px; font-weight: normal; font-family: arial; margin-bottom: 0.5em; margin-top: 0.5em;"></span>
			</a>
		</li>
		<!--galleryEntry 6 -->
		<li class="item">
			<a class="teaser" style="background-color:transparent !important;  font-weight: normal; text-decoration: none;" href="http://www.hrwallingford.com" target="_blank">
		    <img class="overlay" src="http://www.opentelemac.org/modules/mod_animate_hover/images/logo_HRW.png" alt="" />
			<span style="color:grey; font-size: 14px; font-weight: bold; line-height: 18px; font-family: arial; margin-bottom: 0.5em; margin-top: 0.5em; display: block; text-align: center;">HR Wallingford</span>
			<span style="color:white; display: block; font-size: 12px; font-weight: normal; font-family: arial; margin-bottom: 0.5em; margin-top: 0.5em;"></span>
			</a>
		</li>
	</ul>
</div> <!-- ahgalleryOTconsortium -->
</div><!-- footer -->
</div><!-- footer-outer -->

<div class="footer-inner">
      <div class="bar__bottom_footer">
	     <a href="http://www.opentelemac.org/index.php/forum-rules" >Forum rules</a>
         <a href="http://www.opentelemac.org/index.php/licence" >Licence</a>
	     <a href="http://www.opentelemac.org/index.php/privacy" >Privacy</a>
		 <a href="http://www.opentelemac.org/index.php/terms-and-conditions" >Terms &amp; Conditions</a>
	  </div>
</div>
   
<script type="text/javascript">

  var _gaq = _gaq || [];
  _gaq.push(['_setAccount', 'UA-24555872-4']);
  _gaq.push(['_setDomainName', 'opentelemac.org']);
  _gaq.push(['_trackPageview']);

  (function() {
    var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;
    ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';
    var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(ga, s);
  })();

</script>
 
 
   <?php flush()?>
 
<?php /*old includehook*/ @include(dirname(__FILE__).'/footer.html')?>  
    
</body>
</html>
