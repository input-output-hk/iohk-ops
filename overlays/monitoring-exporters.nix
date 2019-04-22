self: super:
{
  nginxMainline = super.nginxMainline.override (oldAttrs: {
    modules = oldAttrs.modules ++ [ super.nginxModules.vts ];
  });

  nginxStable = super.nginxStable.override (oldAttrs: {
    modules = oldAttrs.modules ++ [ super.nginxModules.vts ];
  });
}
