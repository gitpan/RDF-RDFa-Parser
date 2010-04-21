package RDF::RDFa::Parser::Profile::RDF;

use File::Spec;
use HTTP::Cache::Transparent;
use LWP::UserAgent;
use RDF::RDFa::Parser;
use RDF::Trine;

use base qw(RDF::RDFa::Parser::Profile);
use strict;
use 5.008;

our $VERSION = '1.09_03';

BEGIN
{
	HTTP::Cache::Transparent::init({BasePath => File::Spec->tmpdir.'/cache/'});
	
	# This lets RDF::RDFa::Parser find this module.
	push @RDF::RDFa::Parser::Profile::Modules, __PACKAGE__;
}

sub new
{
	my ($class, $uri, $parser) = @_;
	
	my $ua       = LWP::UserAgent->new( agent => __PACKAGE__.' ' );
	my $response = $ua->get($uri);
	
	my $model;
	
	if ($response->code == 200)
	{
		my ($rdfa_opts, $trine);
		
		if ($response->content_type eq 'text/html')
			{ $rdfa_opts = &RDF::RDFa::Parser::OPTS_HTML5; }
		elsif ($response->content_type eq 'application/xhtml+xml')
			{ $rdfa_opts = &RDF::RDFa::Parser::OPTS_XHTML_11; }
		elsif ($response->content_type eq 'image/svg+xml')
			{ $rdfa_opts = &RDF::RDFa::Parser::OPTS_SVG; }
		elsif ($response->content_type eq 'application/atom+xml')
			{ $rdfa_opts = &RDF::RDFa::Parser::OPTS_ATOM; }
		elsif ($response->content_type eq 'application/rdf+xml')
			{ $trine = 'rdfxml'; }
		elsif ($response->content_type eq 'application/json')
			{ $trine = 'rdfjson'; }
		elsif ($response->content_type eq 'application/x-rdf+json')
			{ $trine = 'rdfjson'; }
		elsif ($response->content_type eq 'text/n3')
			{ $trine = 'turtle'; }
		elsif ($response->content_type eq 'text/turtle')
			{ $trine = 'turtle'; }
		elsif ($response->content_type eq 'application/turtle')
			{ $trine = 'turtle'; }
		elsif ($response->content_type eq 'application/x-turtle')
			{ $trine = 'turtle'; }
		elsif ($response->content_type eq 'text/plain')
			{ $trine = 'ntriples'; }
		
		if (defined $rdfa_opts)
		{
			eval 
			{
				my $parser = RDF::RDFa::Parser->new(
					$response->decoded_content, $response->base, $rdfa_opts);
				$parser->consume;
				$model = $parser->graph;
			};
			return undef if $@;
		}
		elsif (defined $trine)
		{
			eval
			{
				$model = RDF::Trine::Model->temporary_model;
				my $parser = RDF::Trine::Parser->new($trine);
				$parser->parse_into_model(
					$response->base, $response->decoded_content, $model);
			};
			return undef if $@;
		}
	}

	if (defined $model)
	{
		my $self = bless { definitions=>0, prefixes=>[], terms=>[] }, $class;
			
		my $term_results   = $model->get_pattern( $self->_term_pattern );
		while (my $row = $term_results->next)
		{
			next unless UNIVERSAL::isa($row->{'term'}, 'RDF::Trine::Node::Literal');
			next unless UNIVERSAL::isa($row->{'uri'}, 'RDF::Trine::Node::Literal');
			
			$self->{definitions}++;
			push @{ $self->{'terms'} },
				[
					$row->{'term'}->literal_value,
					$row->{'uri'}->literal_value,
					0,
					'*',
				];
		}
		
		my $prefix_results = $model->get_pattern( $self->_prefix_pattern );
		while (my $row = $prefix_results->next)
		{
			next unless UNIVERSAL::isa($row->{'prefix'}, 'RDF::Trine::Node::Literal');
			next unless UNIVERSAL::isa($row->{'uri'}, 'RDF::Trine::Node::Literal');
			
			$self->{definitions}++;
			push @{ $self->{'prefixes'} },
				[
					lc $row->{'prefix'}->literal_value,
					$row->{'uri'}->literal_value,
				];
		}
		
		if ($self->{definitions})
		{
			return $self;
		}
	}

	return undef;
}

sub _term_pattern
{
	my ($self) = @_;
	
	my $binding = RDF::Trine::Node::Variable->new('binding');
	
	return RDF::Trine::Pattern->new(
		RDF::Trine::Statement->new(
			$binding,
			RDF::Trine::Node::Resource->new('http://www.w3.org/ns/rdfa#term'),
			RDF::Trine::Node::Variable->new('term'),
			),
		RDF::Trine::Statement->new(
			$binding,
			RDF::Trine::Node::Resource->new('http://www.w3.org/ns/rdfa#uri'),
			RDF::Trine::Node::Variable->new('uri'),
			),
		);
}

sub _prefix_pattern
{
	my ($self) = @_;
	
	my $binding = RDF::Trine::Node::Variable->new('binding');
	
	return RDF::Trine::Pattern->new(
		RDF::Trine::Statement->new(
			$binding,
			RDF::Trine::Node::Resource->new('http://www.w3.org/ns/rdfa#prefix'),
			RDF::Trine::Node::Variable->new('prefix'),
			),
		RDF::Trine::Statement->new(
			$binding,
			RDF::Trine::Node::Resource->new('http://www.w3.org/ns/rdfa#uri'),
			RDF::Trine::Node::Variable->new('uri'),
			),
		);
}

sub get_terms
{
	my ($self) = @_;
	return @{ $self->{'terms'} };
}

sub get_prefixes
{
	my ($self) = @_;
	return @{ $self->{'prefixes'} };
}


1;

__END__

=head1 NAME

RDF::RDFa::Parser::Profile::RDF - profiles written in RDF

=head1 DESCRIPTION

This is the recommended format for profiles in RDFa 1.1.

=head1 SEE ALSO

L<RDF::RDFa::Parser>,
L<RDF::RDFa::Parser::Profile>.

=head1 AUTHOR

Toby Inkster E<lt>tobyink@cpan.orgE<gt>.

=head1 COPYRIGHT

Copyright 2008-2010 Toby Inkster

This library is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

=cut
